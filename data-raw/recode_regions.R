

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


test <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" , "HU12", "DED", "FRK", "FR7"), 
  values = runif(7, 0, 100 ),
  stringsAsFactors = F)

test_df <- data.frame ( 
  geo = nuts_countries, 
  values = 1:length(nuts_countries)) 

dat <- test 


all_geo_codes
recode_nuts <- function( dat, 
                         geo_var = "geo",
                         source_nuts_year = 2016,
                         target_nuts_year = 2013 ) {
  nuts <- NULL
  
  dat <- mutate_if ( dat, is.factor, as.character )
  
  if ( geo_var != "geo" ) {
    dat <- dat %>%
      rename ( geo = !! geo_var )
  }
  
  if ( !'typology' %in% names(dat) ) {
    dat$typology <- NA_character_
  }
  
  utils::data (all_valid_nuts_codes, package ="regions", 
               envir = environment())
  
  codes_in_target_year <- all_valid_nuts_codes %>% 
   filter (nuts == paste0("code_", target_nuts_year)) %>%
    filter (!is.na(geo)) %>%
    select ( -nuts ) %>%
    distinct ( typology, geo ) %>%
    mutate ( geo2 = geo ) %>%
    purrr::set_names ( c("typology", paste0("code_",target_nuts_year),
                         "geo")) 
  
  codes_in_source_year <- all_valid_nuts_codes %>% 
    filter (nuts == paste0("code_", source_nuts_year)) %>%
    filter (!is.na(geo)) %>%
    select ( -nuts ) %>%
    distinct ( typology, geo ) %>%
    mutate ( geo2 = geo ) %>%
    purrr::set_names ( c("typology", paste0("code_",source_nuts_year),
                         "geo")) 
  
  join_by_vars <- names(codes_in_target_year)[names(codes_in_target_year) %in% names(dat)]
  
  if(length(join_by_vars)==0) {
    stop ("Cannot join with valid nuts codes, 'geo' is not present.")
  }
  
  same_coding <- dat %>%   # coded as in target year -------------------
    semi_join ( codes_in_target_year, 
                by = join_by_vars ) %>%
    left_join ( codes_in_target_year, 
                by  = join_by_vars) %>%
    mutate ( geo2 = geo )  %>%
    mutate ( geo3 = geo ) %>%
    select ( one_of("typology", "geo", "geo2", "geo3")) %>%
    purrr::set_names ( c("typology", 
                         "geo",
                    paste0("code_", source_nuts_year ), 
                    paste0("code_", target_nuts_year ))
             ) %>%
    mutate ( typology_change = NA_character_ ) %>%
    left_join ( dat, by = 'geo')
  
  dat_with_source_coding <- dat 
  names(dat_with_source_coding )[
    which(names(dat_with_source_coding ) =="geo")
    ] <- paste0("code_", source_nuts_year )
  
  recoding  <- dat %>%   ## codes are not in the target typology --------
    anti_join ( codes_in_target_year, 
                by = join_by_vars ) %>%
    left_join ( all_valid_nuts_codes, 
                by = join_by_vars ) %>%
    mutate ( nuts = ifelse (is.na(nuts), 
                            yes = "missing",
                            no  = nuts )) %>%
    distinct ( typology, nuts, geo ) %>%
    mutate ( geo2 = geo ) %>%
    mutate ( row  = 1:nrow(.)) %>%
    spread ( nuts, geo2 ) %>%
    select ( one_of ( "typology", "geo", 
                      paste0("code_",source_nuts_year ))) %>%
    left_join ( nuts_changes %>%
                  select ( one_of("typology", 
                                  paste0("code_", source_nuts_year ), 
                                  paste0("code_", target_nuts_year )
                                  )
                           ), 
                by = names(.)[names(.) %in% names(nuts_changes) ] 
                ) %>%
    purrr::set_names ( c("typology", "geo", "source", "target")) %>%
    filter ( stats::complete.cases(.)) %>%
    mutate ( typology_change = paste0( "recoded from ", source ))  
  
  
  group_var <- names
  
  invalid_codes <- dat %>%
    filter ( ! geo %in% return_df$geo ) %>%
    left_join ( all_valid_nuts_codes, 
                by = join_by_vars ) %>%
    mutate ( years = as.numeric(gsub("code_", "" , nuts ))) %>%
    group_by_at ( vars(-one_of("years", "nuts"))) %>%
    summarize ( min_year = min(years, na.rm=TRUE), 
                max_year = max(years, na.rm=TRUE)) %>%
    tidyr::unite ( typology_change, min_year, max_year, sep =   '-') %>%
    mutate ( typology_change = paste0("Used in NUTS ", typology_change)) %>%
    mutate  ( source = NA_character_, target = NA_character_ ) %>%
    ungroup()
  
  
  
  return_df <- bind_cols( recoding %>%
               select (-one_of("source", "target")), 
             recoding %>%
               select (one_of("source", "target")) %>%
               purrr::set_names(paste0("code_", source_nuts_year), 
                                paste0('code_', target_nuts_year))) %>%
    left_join ( dat_with_source_coding , by = paste0("code_", source_nuts_year)) %>%
    bind_rows ( same_coding ) %>%
    bind_rows (bind_cols( invalid_codes %>%
                            select (-one_of("source", "target")), 
                          invalid_codes %>%
                            select (one_of("source", "target")) %>%
                            purrr::set_names(paste0("code_", source_nuts_year), 
                                             paste0('code_', target_nuts_year)) )
    )
  
 }
