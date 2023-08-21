library(tidyverse)
library(shiny)

community_belonging <- read_csv("clean_data/community_belonging.csv")
genders <- unique(community_belonging$gender)
urban_rural <- unique(community_belonging$urban_rural_classification)

ui <- fluidPage(
  fluidRow(
    column(
      width = 2,
      selectInput("gender_input", "Gender", genders)
    ),
    column(
      width = 2,
      selectInput("urban_rural_input", "Urban or Rural", urban_rural)
    )
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)