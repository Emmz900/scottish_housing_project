library(tidyverse)
library(shiny)
library(here)
library(sf)

# Functions -------------
source("R/create_2019_graph.R")
source("R/plot_scores.R")

# Data --------------
community_belonging <- read_csv("clean_data/community_belonging.csv")  %>% 
  mutate(community_belonging = factor(community_belonging, 
                                      levels = c("Not at all strongly", 
                                                 "Not very strongly",
                                                 "Don't know",
                                                 "Fairly strongly",
                                                 "Very strongly")))

neighbourhood_rating <- read_csv("clean_data/neighbourhood_rating.csv") %>% 
  mutate(neighbourhood_rating = factor(neighbourhood_rating,
                                       levels = c("Very poor",
                                                  "Fairly poor",
                                                  "No opinion",
                                                  "Fairly good",
                                                  "Very good")))

data_2019 <- read_csv("clean_data/full_2019_responses.csv") %>% 
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
  dsn = "clean_data/map_data/",
  layer = "pub_las")

spatial_neighbourhood <- read_csv("clean_data/spatial_neighbourhood.csv") 

spatial_neighbourhood_joined <- council_boundaries %>% 
  right_join(spatial_neighbourhood, by = join_by(code == feature_code))

spatial_community <- read_csv("clean_data/spatial_community.csv")

spatial_community_joined <- council_boundaries %>% 
  right_join(spatial_community, by = join_by(code == feature_code))

# Lists ------------------
areas <- sort(unique(community_belonging$area))
years <- sort(unique(neighbourhood_rating$year))
optional_variables <- c("gender", "urban_rural_classification", "simd_quintiles",
                        "type_of_tenure", "household_type", "ethnicity",
                        "walking_distance_to_nearest_greenspace")

