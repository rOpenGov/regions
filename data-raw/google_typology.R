library(regions)
library(tidyverse)

normalize_text <- function(x) {
  
  x <- as.character(x)
  x <- tolower(x)
  x <- str_trim(x, side = "both")
  x <- gsub("\\s", "_", x)
  x <- gsub(":|!|,|;|%","", x)
  x <- gsub("____|___|__", "_", x)
  x
}

### use your own path or save a copy of the Global Mobility Reports
###  into 'data-raw'
###  This file is not synchronized with the GitHub Repo because it
### is large and will slow down gits.

gmr_csv <- read_csv( file.path('data-raw', 
                               'Global_Mobility_Report.csv'))

gmr <- gmr_csv %>%
  set_names ( c("country_code", "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2", 
                "date", "retail", "grocery", 
                "parks", "transit", "workplaces", "residential") ) 

## First joining with valid NUTS codes ------------
## When there is no region name, use the country name
## And preferably the English country name, not the national language one

data("all_valid_nuts_codes", package = 'regions')


nuts_gmr <- all_valid_nuts_codes %>%
  mutate ( country_code = get_country_code(geo)) %>%
  distinct ( country_code ) %>% 
  left_join ( gmr, 
              by = 'country_code' ) %>%
  mutate ( google_region_name_1 = case_when (
    is.na(google_region_name_1) & is.na(google_region_name_2) ~ google_country_name, 
    TRUE ~ google_region_name_1
  )) %>%
  pivot_longer ( cols = c("google_region_name_1", 
                          "google_region_name_2"), 
                 names_to = "google_region_level", 
                 values_to = "google_region_name") %>%
  mutate ( google_name = normalize_text (google_region_name )) %>%
  filter ( !is.na(google_name))

## First results before national corrections ------------
google_region_names <- nuts_gmr %>%
  select ( country_code, google_region_level,
           google_region_name, google_name ) %>%
  filter ( complete.cases(.)) %>%
  distinct_all()

## Help table ------------------------------------------------
## Current regions and their official names in NUTS2016, 
## recent changes, name variants in NUTS2013 and NUTS2010 

data("nuts_changes", package = 'regions')

regions_and_names_2016 <- all_valid_nuts_codes %>%
  mutate ( country_code = get_country_code(geo)) %>%
  filter ( nuts == "code_2016" ) %>%
  rename ( code_2016 = geo ) %>% 
  left_join ( nuts_changes %>%
                select ( typology, code_2016, 
                         geo_name_2016, geo_name_2013, 
                         geo_name_2010, change_2016), 
              by = c('typology', 'code_2016')) %>%
  mutate ( country_name = countrycode::countrycode(country_code, 
                                                   "iso2c", 'country.name')
  ) %>%
  mutate ( google_name  = case_when(
    typology == "country" ~ normalize_text(country_name) , 
    TRUE ~ normalize_text(geo_name_2016)   )
  )
  
##  What was found at first try  ---------------------------------

found_in_nuts <- nuts_gmr %>%
  left_join ( regions_and_names_2016, 
              by = c("country_code", "google_name"))

## Let's check Hungary -------------------------------------------
hungary_names <- regions_and_names_2016 %>%
  filter ( country_code  == "HU") %>%
  select ( google_name)  %>%
  unlist() %>% as.character() %>% sort()

## this will fix the problem 
google_region_names %>%
  select ( country_code, google_name ) %>%
  filter ( country_code == "HU") %>%
  mutate ( corrected_google_name = case_when (
    grepl("moson-sopron", google_name) ~ hungary_names[grepl("moson-sopron", hungary_names)],
    grepl( "hungary|budapest", google_name) ~ google_name,
    grepl( "_county", google_name) ~ gsub("_county", "", 
                                          google_name), 
    TRUE ~ google_name)) %>%
  mutate ( google_name = ifelse( country_code == "HU", 
                                 corrected_google_name, 
                                 google_name)) %>%
    select(google_name) %>%
    left_join ( regions_and_names_2016) 

correct_hungary_names <- function(dat) {
  
  hungary_names <- dat %>%
    select ( country_code, google_name ) %>%
    mutate ( corrected_google_name = case_when (
      grepl("moson-sopron", google_name) ~ hungary_names[grepl("moson-sopron", hungary_names)],
      grepl( "hungary|budapest", google_name) ~ google_name,
      grepl( "_county", google_name) ~ gsub("_county", "", 
                                            google_name), 
      TRUE ~ google_name)) %>%
    mutate ( google_name = ifelse( country_code %in% c("HU"), 
                                   corrected_google_name, 
                                   google_name))
  
  as.character(hungary_names$google_name)
  
} 

nuts_gmr_2 <- nuts_gmr %>%
  mutate ( google_name = correct_hungary_names( dat = .)) 


estonia_names <- regions_and_names_2016 %>%
  filter ( country_code  == "EE") %>%
  select ( google_name)  %>%
  unlist() %>% as.character() %>% sort()

estonia_names 

estonia <- google_region_names %>%
  select ( country_code, google_name ) %>%
  filter ( country_code == "EE") %>%
  mutate ( corrected_google_name = case_when (
    grepl( "_county", google_name) ~ gsub("_county", "-eesti", 
                                          google_name), 
    grepl("ne-eesti", google_name) ~ estonia_names[grepl("ne-eesti", estonia_names)],
    TRUE ~ google_name)) %>%
  mutate ( google_name = ifelse( country_code == "EE", 
                                 corrected_google_name, 
                                 google_name)) %>%
  left_join ( regions_and_names_2016) 