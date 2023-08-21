library(tidyverse)
library(janitor)

council_areas <- clean_names(read_csv("raw_data/council_area_codes.csv"))

council_areas_clean <- council_areas %>% 
  select(ca, ca_name) %>% 
  add_row(ca = "S92000003" ,ca_name = "Scotland")

write_csv(council_areas_clean, "clean_data/council_areas.csv")
