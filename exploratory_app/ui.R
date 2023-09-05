# UI -----------------
ui <- fluidPage(
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # removes minor tick marks from year slider
  tabsetPanel(type = "tabs",
              # Overview Tab -----------------------
              tabPanel(tags$h1("Overview"),
                       fluidRow(
                         tags$h2("Background"),
                         "This shiny app was created to aid with data exploration 
                         during my final project of the CodeClan Professional Data Analysis course.\n",
                         br(),
                         "The project was to explore the factors impacting neighbourhood 
                         ratings and community belonging in Scotland, 
                         using data from the Scottish Household Survey.\n",
                         "The Scottish Household Survey is an annual, cross-sectional 
                         survey of a random selection of people in private residences across Scotland.\n",
                         br(),
                         tags$b("Definitions:"),
                         "SIMD = Scottish Index of Multiple Deprivation",
                         br(),
                         tags$h2("Findings")
                       ),
                       fluidRow(
                         "These plots highlight that there are overall differences in community belonging 
                         between different groups and areas. Woman and pensioners for example tend to rate their community belonging 
                         higher than males, or adult and children households. Those in rural areas also tend to rate their community belonging 
                         higher than those in urban areas.
                         Community belonging is also higher for those that own their house, and those in areas with higher SIMD.",
                         br(),
                         "Explore the other tabs to see how this changes in different areas and years."
                       ),
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
                       ),
                       fluidRow(
                         tags$h2("Data Sources"),
                         "SHS 2013-2019: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fcommunity-belonging---shs",
                         br(),
                         "Council Boundaries: https://data.spatialhub.scot/dataset/local_authority_boundaries-is/resource/d24c5735-0f1c-4819-a6bd-dbfeb93bd8e4",
                         br(),
                         "SHS 2019: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8775"
                       )
                       
              ),
              # Map tab -------------
              tabPanel(tags$h1("Maps"),
                       fluidRow(
                         ## Neighbourhood  -----
                         column(
                           width = 3,
                           plotOutput("neighbourhood_map", width = "100%"),
                         ),
                         ### Neighbourhood tables ------------
                         column(
                           width = 3,
                           tags$h2("Top 5"),
                           tableOutput("neighbourhood_top"),
                           tags$h2("Bottom 5"),
                           tableOutput("neighbourhood_bottom")
                         ),
                         ## Community ----------
                         column(
                           width = 3,
                           plotOutput("community_map"),
                         ),
                         ### Community tables -------
                         column(
                           width = 3,
                           tags$h2("Top 5"),
                           tableOutput("community_top"),
                           tags$h2("Bottom 5"),
                           tableOutput("community_bottom")
                         ),
                       ),
                       fluidRow(
                         ## Neighbourhood change -----
                         column(
                           width = 3,
                           plotOutput("neighbourhood_change")
                         ),
                         ### Neighbourhood change tables --------
                         column(
                           width = 3,
                           tags$h2("Largest Increase"),
                           tableOutput("neighbourhood_change_top"),
                           tags$h2("Largest Decrease"),
                           tableOutput("neighbourhood_change_bottom")
                         ),
                         ## Community change ---------
                         column(
                           width = 3,
                           plotOutput("community_change")
                         ),
                         ### Community change tables --------
                         column(
                           width = 3,
                           tags$h2("Largest Increase"),
                           tableOutput("community_change_top"),
                           tags$h2("Largest Decrease"),
                           tableOutput("community_change_bottom")
                         ),
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
              )
  )
)