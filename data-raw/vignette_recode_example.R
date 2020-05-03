library(regions)
library(dplyr)
library(tidyr)

data ("nuts_changes")

nuts_changes %>%
  mutate ( geo_16 = code_2016, 
           geo_13 = code_2013 ) %>%
  filter ( code_2016 %in% c("FRB", "HU11") | 
             code_2013 %in% c("FR7", "HU10")) %>%
  select ( typology, geo_16, geo_13, start_year,
           code_2013, change_2013,
           code_2016, change_2016 ) %>%
  pivot_longer ( cols = starts_with("code"), 
                 names_to = 'definition', 
                 values_to = 'code') %>%
  pivot_longer ( cols = starts_with("change"), 
                 names_to = 'change', 
                 values_to = 'description')  %>%
  filter (!is.na(description), 
          !is.na(code)) %>%
  select (-change)


example <- nuts_changes %>%
  mutate ( geo_16 = code_2016, 
           geo_13 = code_2013 ) %>%
  filter ( code_2016 %in% c("FRB", "HU11") | 
             code_2013 %in% c("FR7", "HU10", "FR24"))  %>%
  select ( contains("2016"), contains("2013"))

example$geo_name_2016
example$geo_name_2013
