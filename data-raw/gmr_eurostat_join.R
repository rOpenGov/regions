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
  mutate ( country_code = regions::get_country_code(geo)) 


gdp <- eurostat::get_eurostat ( "nama_10r_2gdp", 
                                time_format = "num" ) %>%
  filter ( unit == "MIO_EUR", 
           time  >= 2015 ) %>%
  select ( -all_of("unit")) %>% 
  pivot_wider ( names_from = 'geo', 
                values_from = 'values') %>%
  fill ( names(.)) %>%
  pivot_longer ( cols = -all_of(c( "time")), 
                 names_to = 'geo', 
                 values_to = 'values') %>%
  mutate ( country_code = regions::get_country_code(geo)) %>%
  filter ( time == 2018 ) %>%
  rename ( gdp = values ) %>%
  left_join ( regional_employment %>%
                filter (time == 2018 ), 
              by = c("time", "geo", "country_code")) %>%
  rename ( employment = values ) %>%
  mutate ( gdp_per_employee = gdp*1000000/employment )

gmr <- gmr_csv %>%
  set_names ( c("country_code", "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2", 
                "date", "retail", "grocery", 
                "parks", "transit", "workplaces", "residential") ) 

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
                filter ( nchar(code_2016)>2, 
                         time  == 2019 ), 
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


regional_gdp_data <- gmr %>%
  anti_join( countries, by = c("country_code", "date", 
                               "retail", "grocery", "parks", 
                               "transit", "workplaces", 
                               "residential") ) %>%
  left_join ( google, by = c("country_code", "google_region_name_1") ) %>%
  filter ( !is.na(code_2016)) %>% 
  mutate ( nuts2 = substr(code_2016, 1,4)) %>%
  left_join ( gdp %>% rename ( nuts2 = geo ), 
              by = c("country_code", "nuts2"))  %>%
  select ( -all_of(c("google_region_name_1", "google_region_name_2", 
                     "google_country_name", "employment", "gdp")))  %>%
  group_by ( date,  country_code ) %>%
  summarize_at ( vars(all_of(c("retail", "grocery", "parks", "transit", 
                               "workplaces", "residential"))), 
                 funs(weighted.mean(., w=gdp_per_employee, na.rm=TRUE)))

unique(regional_data$date)

time_length <- gmr %>%
  anti_join( countries, by = c("country_code", "date", 
                               "retail", "grocery", "parks", 
                               "transit", "workplaces", 
                               "residential") ) %>%
  left_join ( google, by = c("country_code", "google_region_name_1") ) %>%
  filter ( !is.na(code_2016)) %>% 
  mutate ( nuts2 = substr(code_2016, 1,4)) %>%
  group_by ( nuts2 ) %>%
  summarize ( min_date = lubridate::ymd(min(date)),
              max_date = lubridate::ymd(max(date))) %>%
  ungroup() %>%
  mutate (  days = lubridate::days(max_date-min_date) ) %>%  #as interval
  mutate (  days = lubridate::day(days))                     #as number

all(time_length$days == 71)

### weigthed GVA / employee 
regional_gdp_data_2 <- gmr %>%
  anti_join( countries, by = c("country_code", "date", 
                               "retail", "grocery", "parks", 
                               "transit", "workplaces", 
                               "residential") ) %>%
  left_join ( google, by = c("country_code", "google_region_name_1") ) %>%
  filter ( !is.na(code_2016)) %>% 
  mutate ( nuts2 = substr(code_2016, 1,4)) %>%
  left_join ( gdp %>% rename ( nuts2 = geo ), 
              by = c("country_code", "nuts2"))  %>%
  select ( -all_of(c("google_region_name_1", "google_region_name_2", 
                     "google_country_name", "gdp" )))  %>%
  select ( -all_of(c("parks", "transit", "residential"))) %>%
  mutate_at ( vars(all_of(c("retail", "grocery", "workplaces"))), 
                 funs( (./100)*(gdp_per_employee*employment*(1/365)))) %>%
  group_by ( country_code ) %>%
  summarize_at ( vars(all_of(c("retail", "grocery",  
                               "workplaces"))), 
                 funs(mean(., na.rm=TRUE)*71)) %>%
  arrange ( workplaces ) %>%
  left_join ( gdp %>% filter ( nchar(geo)==2, 
                               time == 2018 ) %>%
                select ( country_code,  gdp ), 
              by = c("country_code")) %>%
  mutate ( proportional_gdp_loss = workplaces / (gdp*1000000), 
           proportional_retail_loss = retail / (gdp*1000000), 
           proportional_grocery_loss = grocery / (gdp*1000000)) %>%
  arrange ( proportional_gdp_loss )

library(ggflags)
regional_gdp_data_2 %>%
  ggplot ( data  = ., 
           aes ( x = forcats::fct_reorder(country_code,proportional_gdp_loss ), 
                 y = proportional_gdp_loss)) +
  geom_col() +
  coord_flip() +
  geom_hline ( aes ( yintercept = mean(proportional_gdp_loss))) +
  scale_y_continuous( labels = scales::percent) +
  labs ( x = "", y = "Proportional GDP loss severity", 
         title = "Supply Side Effects: Absentism From Workplaces")



