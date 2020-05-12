library(regions)
library(dplyr)
library(tidyr)
library(purrr)

gmr_csv <- readr::read_csv( file.path('data-raw', 
                               'Global_Mobility_Report.csv'))

population <- eurostat::get_eurostat ( 'demo_r_pjangrp3', 
                                       time_format = 'num') 

employment <- eurostat::get_eurostat("lfst_r_lfe2emp", 
                                     time_format = "num")

population_total  <- population %>%
  filter ( sex == "T", age == "TOTAL" ) %>%
  filter ( time >= 2015 ) %>%
  select ( -all_of(c("sex", "age", "unit"))) %>%
  pivot_wider ( names_from = 'geo', 
                values_from = 'values') %>%
  fill ( names(.)) %>%
  pivot_longer ( cols = -all_of(c( "time")), 
                 names_to = 'geo', 
                 values_to = 'values') %>%
  mutate ( country_code = regions::get_country_code(geo)) %>%
  filter ( time == 2019 )

regional_employment <- employment %>%
  filter ( sex == "T", age == "Y_GE15") %>%
  filter ( unit == "THS") %>%
  mutate ( values = values*1000) %>%
  select (-all_of(c("unit", "sex", "age"))) %>%
  pivot_wider ( names_from = 'geo', 
                values_from = 'values') %>%
  fill ( names(.)) %>%
  pivot_longer ( cols = -all_of(c( "time")), 
                 names_to = 'geo', 
                 values_to = 'values') %>%
  mutate ( country_code = regions::get_country_code(geo)) %>%
  filter ( time == 2019 )

unique ( regional_employment$unit)

gmr <- gmr_csv %>%
  set_names ( c("country_code", "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2", 
                "date", "retail", "grocery", 
                "parks", "transit", "workplaces", "residential") ) 

View ( head ( gmr))

data("google_nuts_matchtable", package = "regions")

google <- google_nuts_matchtable %>%
  filter ( typology != 'invalid typology') %>%
  filter ( code_2016 != "PL71") %>%
  tidyr::pivot_wider( names_from = "google_region_level", 
                      values_from = "google_region_name") 

countries <- gmr %>%
  filter ( is.na(google_region_name_1) & is.na(google_region_name_2)) %>%
  select ( - all_of(c("google_region_name_1", "google_region_name_2"))) %>%
  rename ( country_name = google_country_name ) %>%
  left_join ( population_total %>%
                rename ( population = values ) %>%
                filter ( nchar(geo)==2)
              ) %>%
  filter ( !is.na(population))

regional_data <- gmr %>%
  anti_join( countries, by = c("country_code", "date", 
                               "retail", "grocery", "parks", 
                               "transit", "workplaces", 
                               "residential") ) %>%
  left_join ( google, by = c("country_code", "google_region_name_1") ) %>%
  filter ( !is.na(code_2016)) %>%
  left_join ( regional_employment %>%
                rename ( employment = values, 
                         code_2016 = geo  ) %>%
                filter ( nchar(code_2016)>2), 
              by = c("country_code", "code_2016")
  ) %>%
  left_join ( population_total %>%
                rename ( population = values, 
                         code_2016 = geo  ) %>%
                filter ( nchar(code_2016)>2), 
              by = c("country_code", "code_2016", "time")
  )  %>% 
  filter ( !is.na(employment), 
           !is.na(population)) %>%
  select ( -all_of(c("google_region_name_2", "time"))) %>%
  mutate ( employed_rate = employment / population )


weighted_employment  <-  regional_data  %>%
  select ( -all_of(c("google_region_name_1", 
                     "google_country_name"))) %>%
  group_by ( date,  country_code ) %>%
  summarize_at ( vars(all_of(c("retail", "grocery", "parks", "transit", 
                            "workplaces", "residential"))), 
                 funs(weighted.mean(., w=employed_rate, na.rm=TRUE)))



