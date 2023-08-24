library(tidyverse)
library(sf)

neighbourhood_rating <- read_csv("clean_data/neighbourhood_rating.csv")
community_belonging <- read_csv("clean_data/community_belonging.csv")
council_boundaries <- st_read(
  dsn = here("clean_data/map_data/"),
  layer = "pub_las")

aggregated_neighbourhood <- neighbourhood_rating %>% 
  filter(measurement == "Percent", gender != "All") %>% 
  group_by(feature_code, area, year, gender) %>% 
  summarise(total_score = sum(score)) %>% 
  ungroup() %>% 
  group_by(feature_code, area, year) %>% 
  summarise(avg_score = mean(total_score))

change_in_score <- aggregated_neighbourhood %>% 
  filter(year %in% c(2013, 2019)) %>% 
  group_by(feature_code) %>% 
  summarise(diff = diff(avg_score)) %>%
  arrange(diff)

joined_neighbourhood <- aggregated_neighbourhood %>% 
  group_by(feature_code) %>% 
  summarise(score = mean(avg_score)) %>% 
  full_join(change_in_score)

aggregated_neighbourhood %>% 
  left_join(council_boundaries, by = join_by(feature_code == code))

write_csv(aggregated_neighbourhood, "clean_data/spatial_neighbourhood.csv")

# FIX

aggregated_community <- community_belonging %>% 
  filter(measurement == "Percent", gender != "All") %>% 
  group_by(feature_code, area, year, gender) %>% 
  summarise(total_score = sum(score)) %>% 
  ungroup()
group_by(feature_code, area, year) %>% 
  summarise(avg_score = mean(total_score))

aggregated_community %>% 
  left_join(council_boundaries, by = join_by(feature_code == code))

write_csv(aggregated_community, "clean_data/spatial_community.csv")