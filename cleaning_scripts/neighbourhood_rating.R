library(tidyverse)
library(janitor)

council_areas <- read_csv("clean_data/council_areas.csv")
neighbourhood_rating <- clean_names(read_csv("raw_data/neighbourhood_rating.csv"))

# Join council names to data -----------
neighbourhood_rating_joined <- neighbourhood_rating %>% 
  left_join(council_areas, by = c("feature_code" = "ca"))

# Cleaning ----------------------
# There is no missing data in this dataset
neighbourhood_rating_clean <- neighbourhood_rating_joined %>% 
  # factor neighbourhood_rating column
  mutate(neighbourhood_rating = factor(neighbourhood_rating, 
                                      levels = c("Very poor", 
                                                 "Fairly poor",
                                                 "No opinion",
                                                 "Fairly good",
                                                 "Very good"))
  ) %>% 
  
  # it will be useful to have a neighbourhood score for each row (percentage x rating)
  mutate(score = case_when(
    neighbourhood_rating == "Very poor" ~ value/100 * 1,
    neighbourhood_rating == "Fairly poor" ~ value/100 * 2,
    neighbourhood_rating == "No opinion" ~ 3,
    neighbourhood_rating == "Fairly good" ~ value/100 * 4,
    neighbourhood_rating == "Very good" ~ value/100 * 5
  )) %>% 
  
  # the units are all "Percent Of Adults" therefore `units` is not a necessary column
  select(-units) %>% 
  rename("area" = ca_name) %>% 
  rename("year" = date_code)

# Write clean data -----------------
write_csv(neighbourhood_rating_clean, "clean_data/neighbourhood_rating.csv")