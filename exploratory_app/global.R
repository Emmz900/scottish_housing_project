library(tidyverse)
library(shiny)
library(here)

source(here("analysis_scripts_and_functions/create_2019_graph.R"))

community_belonging <- read_csv(here("clean_data/community_belonging.csv"))  %>% 
  mutate(community_belonging = factor(community_belonging, 
                                      levels = c("Not at all strongly", 
                                                 "Not very strongly",
                                                 "Don't know",
                                                 "Fairly strongly",
                                                 "Very strongly")))

neighbourhood_rating <- read_csv(here("clean_data/neighbourhood_rating.csv")) %>% 
  mutate(neighbourhood_rating = factor(neighbourhood_rating,
                                       levels = c("Very poor",
                                                  "Fairly poor",
                                                  "No opinion",
                                                  "Fairly good",
                                                  "Very good")))

areas <- sort(unique(community_belonging$area))
years <- sort(unique(neighbourhood_rating$year))
optional_variables <- c("gender", "urban_rural_classification", "simd_quintiles",
                        "type_of_tenure", "household_type", "ethnicity",
                        "walking_distance_to_nearest_greenspace")

ui <- fluidPage(
  tabsetPanel(type = "tabs",
              tabPanel("Neighbourhood Rating",
                       fluidRow(
                         column(
                           width = 2,
                           selectInput("area_input_n", "Area", areas,
                                       selected = "Scotland")
                         ),
                         column(
                           width = 2,
                           selectInput("variable_input_n", "Group",
                                       optional_variables,
                                       selected = "walking_distance_to_nearest_greenspace")
                         ),
                         column(
                           width = 2,
                           sliderInput("year_input_n", "Year",
                                       min = 2013, max = 2019, value = 2019,
                                       step = 1, sep = "", ticks = FALSE)
                         )
                       ),
                       fluidRow(
                         plotOutput("score_plot_n")
                       ),
                       fluidRow(
                         plotOutput("percentage_plot_n")
                       )
              ),
              tabPanel("Community Belonging",
                       fluidRow(
                         column(
                           width = 2,
                           selectInput("area_input_c", "Area", areas,
                                       selected = "Scotland")
                         ),
                         column(
                           width = 2,
                           selectInput("variable_input_c", "Group",
                                       optional_variables,
                                       selected = "walking_distance_to_nearest_greenspace")
                         ),
                         column(
                           width = 2,
                           sliderInput("year_input_c", "Year",
                                       min = 2013, max = 2019, value = 2019,
                                       step = 1, sep = "", ticks = FALSE)
                         )
                       ),
                       fluidRow(
                         plotOutput("score_plot_c")
                       ),
                       fluidRow(
                         plotOutput("percentage_plot_c")
                       )
              )
  )
)

server <- function(input, output, session) {
  
  output$score_plot_n <- renderPlot({
    neighbourhood_rating %>% 
      filter(area == input$area_input_n, measurement == "Percent",
             get(input$variable_input_n) != "All") %>% 
      summarise(mean_score = sum(score),
                .by = c(year, input$variable_input_n)) %>% 
      ggplot(aes(year, mean_score, colour = get(input$variable_input_n))) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(2013, 2019, 1)) +
      #scale_y_continuous(limits = c(0, 5)) +
      geom_vline(xintercept = input$year_input_n,
                 colour = "black", alpha = 0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 16)) +
      labs(
        title = "Neighbourhood Rating Score",
        x = "Year",
        y = "Average Score (-1 to 1)",
        colour = "Classification"
      )
  })
  
    output$percentage_plot_n <- renderPlot({
    neighbourhood_rating %>% 
      filter(area == input$area_input_n, measurement == "Percent",
             get(input$variable_input_n) != "All",
             year == input$year_input_n) %>% 
      select(input$variable_input_n, neighbourhood_rating, value) %>% 
      ggplot(aes(neighbourhood_rating, value, fill = get(input$variable_input_n))) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste(value, "%")), vjust = -1, position = position_dodge(0.9)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 16)) +
      labs(
        title = "Neighbourhood Rating Responses",
        x = "Neighbourhood Rating",
        y = "Percentage of survey responses",
        fill = "Classification"
      )
  })
  
  output$score_plot_c <- renderPlot({
    community_belonging %>% 
      filter(area == input$area_input_c, measurement == "Percent",
             get(input$variable_input_c) != "All") %>% 
      summarise(total_score = sum(score),
                .by = c(year, input$variable_input_c)) %>% 
      ggplot(aes(year, total_score, colour = get(input$variable_input_c))) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(2013, 2019, 1)) +
      geom_vline(xintercept = input$year_input_c,
                 colour = "black", alpha = 0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 16)) +
      labs(
        title = "Community Belonging Score",
        x = "Year",
        y = "Average Score (-1 to 1)",
        colour = "Classification"
      )
  })
  
  
  output$percentage_plot_c <- renderPlot({
    community_belonging %>% 
      filter(area == input$area_input_c, measurement == "Percent",
             get(input$variable_input_c) != "All",
             year == input$year_input_c) %>% 
      select(input$variable_input_c, community_belonging, value) %>% 
      ggplot(aes(community_belonging, value, fill = get(input$variable_input_c))) +
      geom_col(position = "dodge") +
      geom_text(aes(label = paste(value, "%")), vjust = -1, position = position_dodge(0.9)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 16)) +
      labs(
        title = "Community Belonging Responses",
        x = "Community Belonging",
        y = "Percentage of survey responses",
        fill = "Classification"
      )
  })
}

shinyApp(ui, server)