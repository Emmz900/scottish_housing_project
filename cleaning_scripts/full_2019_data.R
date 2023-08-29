library(tidyverse)
library(janitor)
library(here)

full_data_2019 <- read.delim(here("raw_data/tab/shs2019_social_public.tab"))

data_small <- full_data_2019 %>% 
  filter(rand_ok == 1) %>% 
  select(council, # council area
         randage, # age
         randgender, # gender
         RANDETH2012, # ethnicity
         tenure_harm, #tenure_type
         hhtype_new, # household type (1, 2, 6, 7 = adult, 3, 4, 5 = children, 8 = pensioner)
         greenfar13, # how far to nearest green or blue space
         MD20QUIN, # SIMD (1 = most, 5 = least)
         SHS_2CLA, # urban_rural
         rb1, # neighbourhood rating
         commbel # community belonging
  )

data_clean <- data_small %>% 
  rename("ethnicity" = "RANDETH2012") %>% 
  rename("simd" = "MD20QUIN") %>% 
  rename("urban_rural" = "SHS_2CLA") %>% 
  mutate(
    household_type = factor(case_when(
      hhtype_new %in% c(1, 2, 6, 7) ~ "Adult",
      hhtype_new %in% c(3, 4, 5) ~ "Children",
      hhtype_new == 8 ~ "Pensioner"
    )),
    gender = factor(case_when(
      randgender == 1 ~ "Male",
      randgender == 2 ~ "Female",
      .default = "Other/Refused"
    )),
    ethnicity = factor(case_when(
      ethnicity == 1 ~ "White",
      ethnicity == 2 ~ "Other",
      .default = "Refused/Unknown"
    )),
    tenure = factor(case_when(
      tenure_harm == 1 ~ "Owned Outright",
      tenure_harm == 2 ~ "Owned Loan/Mortgage",
      tenure_harm == 3 | tenure_harm == 4  ~ "Social Rented",
      tenure_harm == 5 ~ "Private Rented",
      tenure_harm == 6 ~ "Other",
      .default = "Refused/Unknown"
    )),
    greenspace = factor(case_when(
      greenfar13 == 1 ~ "5 mins or less",
      greenfar13 == 2 ~ "6-10 mins",
      greenfar13 == 3 ~ "11-20 mins",
      greenfar13 == 4 ~ "21-30 mins",
      greenfar13 == 5 ~ "More than 30",
      greenfar13 == 6 ~ "Don't know"
    )),
    urban_rural = factor(case_when(
      urban_rural == 1 ~ "Urban",
      urban_rural == 2 ~ "Rural"
    )),
    neighbourhood_rating = factor(case_when(
      rb1 == 1 ~ "Very good",
      rb1 == 2 ~ "Fairly good",
      rb1 == 3 ~ "Fairly poor",
      rb1 == 4 ~ "Very poor",
      rb1 == 5 ~ "No opinion"
    )),
    community_belonging = factor(case_when(
      commbel == 1 ~ "Very strongly",
      commbel == 2 ~ "Fairly strongly",
      commbel == 3 ~ "Not very strongly",
      commbel == 4 ~ "Not at all strongly",
      commbel == 5 ~ "Don't know"
    )),
    council = factor(council)
  ) %>% 
  select(-c(hhtype_new, randgender, tenure_harm, greenfar13, rb1, commbel))

write_csv(data_clean, "clean_data/full_2019_responses.csv")