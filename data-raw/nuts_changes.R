library(dplyr)
library(tidyr)
nuts_changes <- readRDS(file.path("data-raw", "nuts_changes.rds"))

nuts_recoded <- nuts_changes  %>%
  pivot_longer ( cols = starts_with("change"), 
                 names_to = "change_year", 
                 values_to = "changes") %>%
  filter ( !is.na(changes) ) %>%
  mutate ( changes  = tolower(changes) ) %>%
  mutate ( change_year = as.numeric(gsub("change_", "", change_year))) %>%
  filter ( ! grepl("boundary", changes)) %>%
  filter (  grepl("recode|code change", changes )) %>%
  mutate ( recoded = TRUE ) %>%
  select ( -starts_with("geo"), -changes, -recoded ) %>%
  pivot_longer ( cols = starts_with("code"), 
                 names_to = 'nuts', 
                 values_to = 'geo') %>%
  mutate ( nuts_year = as.numeric(gsub("code_", "", nuts))) %>%
  filter ( ! is.na(geo), 
           ! grepl("created in|discontinued", geo)) %>%
  select ( geo, typology, nuts_year, change_year, start_year, end_year ) %>%
  mutate ( iso2c = substr(geo,1,2)) %>%
  mutate ( iso2c = case_when ( 
    iso2c == "EL" ~ "GR", 
    iso2c == "UK" ~ "GB", 
    TRUE ~ iso2c)) 



usethis::use_data ( nuts_recoded, overwrite = TRUE ) 
