library(tidyverse)
library(janitor)

green_spaces <- clean_names(read_csv("raw_data/green_spaces.csv"))
council_areas <- read_csv("clean_data/council_areas.csv")

# Join council names to data -----------
green_spaces_joined <- green_spaces %>% 
  left_join(council_areas, by = c("feature_code" = "ca"))

# Cleaning ----------------------