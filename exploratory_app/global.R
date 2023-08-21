library(tidyverse)
library(shiny)
library(here)

community_belonging <- read_csv(here("clean_data/community_belonging.csv"))
areas <- unique(community_belonging$area)
optional_variables <- c("gender", "urban_rural_classification", "simd_quintiles",
                        "type_of_tenure", "household_type", "ethnicity",
                        "walking_distance_to_nearest_greenspace")

ui <- fluidPage(
  fluidRow(
    column(
      width = 2,
      selectInput("area_input", "Area", areas)
    ),
    column(
      width = 2,
      selectInput("variable_input", "Group", optional_variables)
    ),
  ),
  fluidRow(
      plotOutput("score_plot")
  )
)

server <- function(input, output, session) {
  output$score_plot <- renderPlot({
    community_belonging %>% 
      filter(area == input$area_input, measurement == "Percent",
             get(input$variable_input) != "All") %>% 
      summarise(mean_score = mean(belonging_score),
                .by = c(year, input$variable_input)) %>% 
      ggplot(aes(year, mean_score, colour = get(input$variable_input))) +
      geom_line()
  })
}

shinyApp(ui, server)