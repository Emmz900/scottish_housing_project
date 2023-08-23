library(tidyverse)
library(janitor)

council_areas <- read_csv("clean_data/council_areas.csv")
community_belonging <- clean_names(read_csv("raw_data/community_belonging.csv"))

# Join council names to data -----------
community_belonging_joined <- community_belonging %>% 
  left_join(council_areas, by = c("feature_code" = "ca"))

# Cleaning ----------------------
# There is no missing data in this dataset
community_belonging_clean <- community_belonging_joined %>% 
  # factor community_belonging column
  mutate(community_belonging = factor(community_belonging, 
                                      levels = c("Not at all strongly", 
                                                 "Not very strongly",
                                                 "Don't know",
                                                 "Fairly strongly",
                                                 "Very strongly"))
         ) %>% 
  
  # it will be useful to have a community belonging score for each row (percentage x rating)
  mutate(score = case_when(
    community_belonging == "Not at all strongly" ~ value/100 * -1,
    community_belonging == "Not very strongly" ~ value/100 * -0.5,
    community_belonging == "Don't know" ~ value/100 * 0,
    community_belonging == "Fairly strongly" ~ value/100 * 0.5,
    community_belonging == "Very strongly" ~ value/100 * 1
  )) %>% 
  
  # the units are all "Percent Of Adults" therefore `units` is not a necessary column
  select(-units) %>% 
  rename("area" = ca_name) %>% 
  rename("year" = date_code)

# Write clean data -----------------
write_csv(community_belonging_clean, "clean_data/community_belonging.csv")
