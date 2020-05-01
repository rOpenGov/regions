

paste (sort(unique ( str_sub(all_geo_codes$geo,1,2 ))), collapse = "', '")

nuts_countries <- c('AL', 'AT', 'BE', 'BG', 'CH', 'CY',
                    'CZ', 'DE', 'DK', 'EE', 'EL', 'ES', 
                    'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 
                    'IS', 'IT', 'LI', 'LT', 'LU', 'LV', 
                    'ME', 'MK', 'MT', 'NL', 'NO', 'PL', 
                    'PT', 'RO', 'RS', 'SE', 'SI', 'SK',
                    'TR', 'UK' )

all_geo_codes <- nuts_changes %>% 
  select ( typology, start_year, end_year, 
                          starts_with ('code')) %>%
  pivot_longer ( cols = starts_with('code'), 
                 names_to = 'nuts', values_to = 'geo') %>%
  filter ( !grepl("part", geo))


nuts_recode <- all_geo_codes %>%
  filter ( !is.na(start_year)) %>%
  full_join ( all_geo_codes %>% filter ( !is.na(end_year)), 
              by = c("typology", "start_year", "end_year", "nuts", "geo")) %>%
  semi_join ( all_geo_codes, 
              by = c("typology", "start_year", "end_year", "nuts", "geo"))


unique ( all_geo_codes$geo)

code  <-

test <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" , "HU12", "DED", "FRK", "FR7"), 
  stringsAsFactors = F)

test_df <- data.frame ( 
  geo = nuts_countries, 
  values = 1:length(nuts_countries)) 

dat <- test_df 
validate_nuts_country <- function ( dat, geo_var = "geo") {
  
  dat <-  mutate_if ( dat, is.factor, as.character )
  if (! "typology" %in% names(dat) ) {
    dat$typology <- NA_character_
  }
  
  original_names <- names(dat)
  
  validate_country_df <- dat %>%
    select ( one_of ( geo_var, "typology" )) %>%
    rename ( !! geo_var := geo ) %>%
    mutate ( iso2c = geo ) %>%
    mutate ( iso2c = case_when ( 
      iso2c == "UK" ~ "GB",
      iso2c == "EL" ~ "GR", 
      iso2c == "XK" ~ "RS", #only to avoid warning
      TRUE ~ iso2c)) %>%
    mutate ( iso3c = countrycode::countrycode(iso2c, "iso2c", "iso3c")) %>%
    mutate ( typology = case_when ( 
      is.na(iso3c)  & nchar(geo) == 2 ~ "invalid_country_code" ,
      !is.na(iso3c) & nchar(geo) == 2 ~ "country", 
      is.na(iso3c)  & nchar(geo) !=2  ~ typology 
      ))
  
  validate_country_df %>%
    select ( one_of(original_names))
}


validate_nuts <- function ( dat = test_df , nuts_year = 2016 ) {
  
  
  validation_df <- try %>%
    filter ( geo %in% dat$geo) %>%
    filter ( nuts == paste0("code_", nuts_year) )  %>%
    right_join ( mutate_if(test_df, is.factor, as.character),
                 by = 'geo') %>%
    mutate ( typology = case_when ( 
      end_year < nuts_year ~ paste0("invalid_discontinued_", end_year),
      end_year < nuts_year ~ paste0("invalid_started_",    start_year), 
      TRUE ~ 'typology')
      ) %>%
    add_count ( geo, values)
}


dat = validation_df 
recode_nuts <- function( dat, 
                         source_nuts_year = 2016,
                         target_nuts_year = 2013 ) {
  
  codes_in_this_year <- all_geo_codes %>% 
   filter (nuts == paste0("code_", target_nuts_year)) %>%
    filter (!is.na(geo)) %>%
    select ( -start_year, -end_year, -nuts ) %>%
    distinct ( typology, geo ) %>%
    mutate ( geo2 = geo ) %>%
    purrr::set_names ( c("typology", paste0("code_",target_nuts_year), "geo")) 
  
  same_coding <- dat %>% 
    semi_join ( codes_in_this_year, 
                by = c("typology", "geo")) %>%
    mutate ( geo2 = geo )  %>%
    mutate ( geo3 = geo ) %>%
    select ( one_of("typology", "geo", "geo2", "geo3")) %>%
    purrr::set_names ( c("typology", 
                         "geo",
                    paste0("code_", source_nuts_year ), 
                    paste0("code_", target_nuts_year ))
             ) 
  
  
  different_coding <- dat %>%
    anti_join ( codes_in_this_year ) %>%
    mutate ( nuts = ifelse (is.na(nuts), "missing",nuts )) %>%
    distinct ( typology, nuts, geo) %>%
    mutate ( geo2 = geo ) %>%
    mutate ( row  = 1:nrow(.)) %>%
    spread ( nuts, geo2 ) %>%
    select ( one_of ( "typology", "geo", 
                      paste0("code_",source_nuts_year ))) %>%
    #filter ( complete.cases(.)) %>%
    left_join ( nuts_changes %>%
                  select ( one_of("typology", 
                                  paste0("code_", source_nuts_year ), 
                                  paste0("code_", target_nuts_year )
                                  )
                           )) %>%
    purrr::set_names ( c("typology", "geo", "source", "target")) %>%
    mutate ( target = ifelse (is.na(target) & geo %in% codes_in_this_year$geo, 
                              yes = geo, 
                              no = target)
             ) %>%
    mutate ( typology = ifelse(is.na(source) & is.na(target), 
                               paste0("invalid_both_", source_nuts_year, "_", 
                               target_nuts_year),
                               typology )) %>%
    purrr::set_names ( c("typology", "geo",
                        paste0("code_", source_nuts_year ), 
                        paste0("code_", target_nuts_year )
    ))
  
  return_df <- bind_rows(  same_coding , different_coding)
  
 }
