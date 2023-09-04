library(tidyverse)
library(shiny)
library(here)

# Functions -------------
source(here("analysis_scripts_and_functions/create_2019_graph.R"))
source(here("analysis_scripts_and_functions/plot_scores.R"))

# Data --------------
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

data_2019 <- read_csv(here("clean_data/full_2019_responses.csv")) %>% 
  mutate(greenspace = factor(greenspace, levels = c(
    "5 mins or less",
    "6-10 mins",
    "11-20 mins",
    "21-30 mins",
    "More than 30",
    "Don't know")),
    community_belonging = factor(community_belonging, 
                                 levels = c("Don't know",
                                            "Not at all strongly", 
                                            "Not very strongly",
                                            "Fairly strongly",
                                            "Very strongly")),
    community_score = case_when(
      community_belonging == "Don't know" ~ 0,
      community_belonging == "Not at all strongly" ~ 1,
      community_belonging == "Not very strongly" ~ 2,
      community_belonging == "Fairly strongly" ~ 3,
      community_belonging == "Very strongly" ~ 4
    ),
    simd = factor(simd)
  ) %>% 
  mutate(across(where(is.character), as.factor))

council_boundaries <- st_read(
  dsn = here("clean_data/map_data/"),
  layer = "pub_las")

spatial_neighbourhood <- read_csv(here("clean_data/spatial_neighbourhood.csv")) 

spatial_neighbourhood_joined <- council_boundaries %>% 
  right_join(spatial_neighbourhood, by = join_by(code == feature_code))

spatial_community <- read_csv(here("clean_data/spatial_community.csv"))

spatial_community_joined <- council_boundaries %>% 
  right_join(spatial_community, by = join_by(code == feature_code))

# Lists ------------------
areas <- sort(unique(community_belonging$area))
years <- sort(unique(neighbourhood_rating$year))
optional_variables <- c("gender", "urban_rural_classification", "simd_quintiles",
                        "type_of_tenure", "household_type", "ethnicity",
                        "walking_distance_to_nearest_greenspace")

# UI -----------------
ui <- fluidPage(
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # removes minor tick marks from year slider
  tabsetPanel(type = "tabs",
              # Overview Tab -----------------------
              tabPanel(tags$h1("Overview"),
                       fluidRow(
                         tags$h2("Background"),
                         "This shiny app was created to aid with data exploration during my final project of the CodeClan Professional Data Analysis course.\n",
                         "The project was to explore the factors impacting neighbourhood ratings and community belonging in Scotland, 
                         using data from the Scottish Household Survey.\n",
                         "The Scottish Household Survey is an annual, cross-sectional survey of a random selection of people in private residences across Scotland.\n",
                         br(),
                         tags$h2("Findings"),
                         fluidRow(
                           column(
                             width = 4,
                             plotOutput("gender_plot"),
                             plotOutput("house_plot")
                           ),
                           column(
                             width = 4,
                             plotOutput("urban_plot"),
                             plotOutput("tenure_plot")
                           ),
                           column(
                             width = 4,
                             plotOutput("simd_plot"),
                             plotOutput("green_plot")
                           )
                         )
                       )
              ),
              
              # Neighbourhood Rating tab ----------------
              tabPanel(tags$h1("Neighbourhood Rating"),
                       fluidRow(
                         ## Area input ------------
                         column(
                           width = 2,
                           selectInput("area_input_n", tags$h2("Area"), areas,
                                       selected = "Scotland")
                         ),
                         ## Variable input ----------------
                         column(
                           width = 2,
                           selectInput("variable_input_n", tags$h2("Group"),
                                       optional_variables,
                                       selected = "walking_distance_to_nearest_greenspace")
                         ),
                         # Year slider input -------------------
                         column(
                           width = 2,
                           sliderInput("year_input_n", tags$h2("Year"),
                                       min = 2013, max = 2019, value = 2019,
                                       step = 1, sep = "", ticks = TRUE)
                         )
                       ),
                       ## Line plot of years -----------
                       fluidRow(
                         plotOutput("score_plot_n")
                       ),
                       ## Col plot of responses ---------------
                       fluidRow(
                         plotOutput("percentage_plot_n")
                       )
              ),
              
              # Community Belonging tab -----------
              tabPanel(tags$h1("Community Belonging"),
                       fluidRow(
                         ## Area input ----------
                         column(
                           width = 2,
                           selectInput("area_input_c", tags$h2("Area"), areas,
                                       selected = "Scotland")
                         ),
                         ## Variable Input --------------
                         column(
                           width = 2,
                           selectInput("variable_input_c", tags$h2("Group"),
                                       optional_variables,
                                       selected = "walking_distance_to_nearest_greenspace")
                         ),
                         ## Year Slider Input ---------------
                         column(
                           width = 2,
                           sliderInput("year_input_c", tags$h2("Year"),
                                       min = 2013, max = 2019, value = 2019,
                                       step = 1, sep = "", ticks = TRUE)
                         )
                       ),
                       ## Line plot of years ------------
                       fluidRow(
                         plotOutput("score_plot_c")
                       ),
                       ## Col plot of responses -----------
                       fluidRow(
                         plotOutput("percentage_plot_c")
                       )
              ),
              
              # Map tab -------------
              tabPanel(tags$h1("Maps"),
                       fluidRow(
                         column(
                           width = 3,
                           plotOutput("neighbourhood_map"),
                           plotOutput("neighbourhood_change")
                         ),
                         column(
                           width = 3,
                           #table of top/bottom areas
                         ),
                         column(
                           width = 3,
                           plotOutput("community_map"),
                           plotOutput("community_change")
                         ),
                         column(
                           width = 3,
                           #table of top/bottom areas
                         ),
                       )
                       
              )
  )
)

# Server ------------------
server <- function(input, output, session) {
  
  ## Summary plots ----------------
  output$gender_plot <- renderPlot({
    plot_scores(gender, "Gender")
  })
  
  output$urban_plot <- renderPlot({
    plot_scores(urban_rural, "Urban or Rural")
  })
  
  output$simd_plot <- renderPlot({
    plot_scores(simd, "SIMD")
  })
  
  output$house_plot <- renderPlot({
    plot_scores(household_type, "Household Type")
  })
  
  output$tenure_plot <- renderPlot({
    plot_scores(tenure, "Type of Tenure")
  })
  
  output$green_plot <- renderPlot({
    plot_scores(greenspace, "Distance to Greenspace")
  })
  
  ## Neighbourhood plots --------------------
  output$score_plot_n <- renderPlot({
    neighbourhood_rating %>% 
      filter(area == input$area_input_n, measurement == "Percent",
             get(input$variable_input_n) != "All") %>% 
      summarise(mean_score = sum(score),
                .by = c(year, input$variable_input_n)) %>% 
      ggplot(aes(year, mean_score, colour = get(input$variable_input_n))) +
      geom_line() +
      geom_point(size = 5) +
      scale_x_continuous(breaks = seq(2013, 2019, 1)) +
      #scale_y_continuous(limits = c(-1, 1)) +
      geom_vline(xintercept = input$year_input_n,
                 colour = "black", alpha = 0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
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
      geom_text(aes(label = paste(value, "%")), vjust = -1,
                position = position_dodge(0.9), size = 8) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Neighbourhood Rating Responses",
        x = "Neighbourhood Rating",
        y = "Percentage of survey responses",
        fill = "Classification"
      )
  })
  
  ## Community plots -----------------
  output$score_plot_c <- renderPlot({
    community_belonging %>% 
      filter(area == input$area_input_c, measurement == "Percent",
             get(input$variable_input_c) != "All") %>% 
      summarise(total_score = sum(score),
                .by = c(year, input$variable_input_c)) %>% 
      ggplot(aes(year, total_score, colour = get(input$variable_input_c))) +
      geom_line() +
      geom_point(size = 5) +
      scale_x_continuous(breaks = seq(2013, 2019, 1)) +
      geom_vline(xintercept = input$year_input_c,
                 colour = "black", alpha = 0.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
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
      geom_text(aes(label = paste(value, "%")), vjust = -1,
                position = position_dodge(0.9), size = 8) +
      scale_y_continuous(limits = c(0, 100)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            text = element_text(size = 24)) +
      labs(
        title = "Community Belonging Responses",
        x = "Community Belonging",
        y = "Percentage of survey responses",
        fill = "Classification"
      )
  })
  
  ## Maps ------------------------
  output$neighbourhood_map <- renderPlot({
    ggplot(spatial_neighbourhood_joined, aes(fill = score)) +
      geom_sf() +
      scale_fill_distiller(palette = "PuBu", direction = 1) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank()) +
      labs(
        title = "Average Neighbourhood Ratings",
        subtitle = "Scale is -1 to 1",
        fill = "Score"
      )
  })
  
  output$community_map <- renderPlot({
    ggplot(spatial_community_joined, aes(fill = score)) +
      geom_sf() +
      scale_fill_distiller(palette = "PuBu", direction = 1) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank()) +
      labs(
        title = "Average Community Belonging",
        subtitle = "Scale is -1 to 1",
        fill = "Score"
      )
  })
  
  output$neighbourhood_change <- renderPlot({
    ggplot(spatial_neighbourhood_joined, aes(fill = diff)) +
      geom_sf() +
      scale_fill_gradient2(low = "#e9a3c9", mid = "#f7f7f7", high = "#a1d76a") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank()) +
      labs(
        title = "Change in Neighbourhood Ratings",
        subtitle = "2013-2019",
        fill = "Change"
      )
  })
  
  output$community_change <- renderPlot({
    ggplot(spatial_community_joined, aes(fill = diff)) +
      geom_sf() +
      scale_fill_gradient2(low = "#e9a3c9", mid = "#f7f7f7", high = "#a1d76a") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank()) +
      labs(
        title = "Change in Community Belonging",
        subtitle = "2013-2019",
        fill = "Change"
      )
  })
  
}

shinyApp(ui, server)