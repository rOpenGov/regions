library(eurostat)
library(regions)
library(dplyr)
library(tidyverse)

regional_gdp <- eurostat::get_eurostat ( "tgs00003", time_format = "num" ) %>%
  mutate ( country = regions::get_country_code(geo)) %>%
  dplyr::filter ( country == "FR")

population <- eurostat::get_eurostat ( "tgs00096", time_format = "num" ) %>%
  mutate ( country = regions::get_country_code(geo)) %>%
  dplyr::filter ( country == "FR", 
                  sex == "T",     #TOTAL without division by sex
                  age == "TOTAL") #TOTAL without division by age

alt_regional_gdp <- eurostat::get_eurostat ( "nama_10r_2gdp",
                                             time_format = "num" ) %>%
  mutate ( country = regions::get_country_code(geo)) %>%
  dplyr::filter ( unit == "MIO_EUR", 
                  country == "FR" ) %>%
  dplyr::select ( all_of(c("geo", "time", "values")))
 

download.file('https://www.insee.fr/fr/statistiques/fichier/1893220/PIB_1990_2015_regions_diffusion.xls', 
              destfile = file.path('data-raw', 
                                   'regional_gdp_fr.xlx'), 
              mode = 'wb')  #for Windows machines

regional_gdp_france <- readxl::read_excel(
  path = file.path('data-raw', 
  'regional_gdp_fr.xlx'), 
  sheet = 'PIB en valeur 1990-2015', 
  skip = 4, 
  na = "n.d") %>%
  dplyr::filter ( !is.na("Région")) %>%
  purrr::set_names ( c("region", paste0("Y", 1990:2015))) %>%
  tidyr::pivot_longer( cols = starts_with("Y"), 
                       names_to = 'time', 
                       values_to = 'values') %>%
  dplyr::mutate ( time  = as.numeric(gsub("Y", "", time)) )


comparison <- regional_gdp_france %>%
  dplyr::filter ( region %in% c("Réunion", "Guadeloupe", "Martinique"), 
                  time > 2012, 
                  time < 2015)

gdp_labelled <- eurostat::get_eurostat ( "nama_10r_2gdp",
                                         time_format = "num" ) %>%
  eurostat::label_eurostat( fix_duplicated = TRUE) %>%
  dplyr::filter ( unit == "Million euro", 
                  time == 2016) %>%
  dplyr::select ( all_of(c("geo", "time", "values")) ) %>%
  dplyr::filter ( geo %in% c("La Réunion", "Guadeloupe", "Martinique")) %>%
  dplyr::rename ( gdp = values )

population_labelled <- eurostat::get_eurostat ("tgs00096",
                                               time_format = "num" ) %>%
  eurostat::label_eurostat( fix_duplicated = TRUE) %>%
  dplyr::filter ( grepl("Réunion|Guadeloupe|Martinique", geo), 
                  sex == "Total",   
                  age == "Total", 
                  time %in% c(2008, 2012, 2016) ) %>%
  dplyr::select(geo, time, values) %>%
  dplyr::rename( population = values )

save ( regional_gdp, alt_regional_gdp, gdp_labelled, 
       population, population_labelled,
       regional_gdp_france, file = 
         file.path('data-raw', 'regional_gdp_blogpost.rda'))

regions::all_valid_nuts_codes()

regions::validate_nuts_regions(population)

guadeloupe <-  nuts_changes %>%
  filter ( grepl ( "Guadeloupe", geo_name_2016) ) %>%
  select ( contains(c("2008", "2010", "2013", "2016")) ) 



martinique <-  nuts_changes %>%
  filter ( grepl ( "Martinique", geo_name_2016) ) %>%
  select ( contains(c("2008", "2010", "2013", "2016")) ) 

population %>%
  regions::validate_nuts_regions() %>%
  filter ( geo %in% c("FRY1", "FRY4"), 
           time %in% c(2008,2012,2016)) 
  

regional_gdp  %>% 
  select ( geo, time, values )  %>%
  rename ( gdp = values )  %>%
  filter ( geo %in% c("FRY1", "FRY4"), 
           time %in% c(2008,2012,2016)) %>%
  bind_rows (pib %>%
               mutate ( geo = case_when (
                 geo == "Guadeloupe" ~ 'FRY10', 
                 TRUE ~ "FRY40"
               )) ) %>%
  validate_nuts_regions()
  
  
  left_join ( 
    population %>%
      select ( all_of(c("geo", "time", "values"))) %>%
      filter ( geo %in% c(guadeloupe_codes, 
                          reunion_codes), 
               time %in% c(2008,2012,2016)) %>%
      arrange(geo) %>%
      regions::validate_nuts_regions(nuts_year = 2016) %>%
      filter ( valid_2016 ) %>%
      select ( all_of(c("geo", "time", "values"))) %>%
      dplyr::rename( population = values ), 
    by = c("geo", "time")
    ) %>%
  mutate ( gdp_capita = gdp*1000000/population)



population %>%
  filter ( geo %in% c(guadeloupe_codes), 
           time %in% c(2008,2012,2016)) %>%
  arrange(geo)


  
  

