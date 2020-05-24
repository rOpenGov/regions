library(regions)
library(tidyverse)

#' @family recode functions
#' @importFrom dplyr mutate select distinct distinct_at filter bind_rows
#' @importFrom dplyr rename arrange
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#' 

impute_down_nuts <- function (dat, nuts_year = 2016 ) {
  . <- geo <- country_code <- NULL
  
  validated <- dat %>%
    validate_nuts_regions( nuts_year = nuts_year ) 
  
  countries_present <- validated %>%
    mutate ( country_code = regions::get_country_code(geo)) %>%
    select ( country_code ) %>%
    distinct ( country_code ) %>%
    unlist() %>% as.character() %>% sort()
  
  all_valid_nuts_codes_year <- all_valid_nuts_codes %>%
    filter ( nuts     ==  paste0("code_", nuts_year) , 
             typology == "nuts_level_3") %>%
    mutate ( 
      ## start with smallest hierarchival unit, which is NUTS3
      country_code = get_country_code (geo), 
      row_id       = 1:nrow(.)
    ) %>%
    pivot_wider ( ., names_from = "typology", 
                  values_from = 'geo') %>%
    mutate ( 
      nuts_level_2 = substr(nuts_level_3, 1, 4), 
      nuts_level_1 = substr(nuts_level_3, 1, 3), 
      country      = substr(nuts_level_3, 1, 2), 
    ) %>%
    select ( -all_of("row_id") )
  
  full_code_table <- all_valid_nuts_codes_year %>%   ## all the regions of the countries present in the datatable
    filter ( country_code %in% countries_present ) 
  
  ##method
  
  ## Create all possible imputations from NUTS0 >> NUTS1 >> NUTS2 >> NUTS3
  potentially_imputed_from_country <- full_code_table %>%
    filter ( country_code %in% countries_present ) %>%
    dplyr::rename ( geo  = country_code ) %>%
    left_join ( mixed_nuts_example, by = 'geo') %>%
    filter ( !is.na(values) ) %>%
    mutate ( method = paste0(
      "imputed from country ", 
      geo, " [", method, "]")
    ) 
  
  ## Create all possible imputations from NUTS1 >> NUTS2 >> NUTS3
  potentially_imputed_from_nuts_1 <- full_code_table %>%
    filter ( nuts_level_1 %in% validated_nuts_1$geo ) %>%
    rename ( geo = nuts_level_1 ) %>%
    left_join ( mixed_nuts_example, by = 'geo' ) %>%
    mutate ( method = paste0(
      "imputed from NUTS1 ", 
      geo, " [", method, "]")
    ) %>%
    select ( all_of(c("nuts_level_2", "values", "method"))) %>%
    dplyr::rename ( geo = nuts_level_2 )
  
  ## Now add potential imputations from NUTS0 if not present in NUTS1
  imputed_from_nuts_1 <- potentially_imputed_from_country %>%
    distinct_at  ( tidyselect::all_of (c("nuts_level_2", "values", 
                                         "method")))  %>%
    rename ( geo = nuts_level_2 ) %>%
    filter ( ! geo %in% potentially_imputed_from_nuts_1$geo ) %>%
    bind_rows ( potentially_imputed_from_nuts_1  )
  
  ## Create all possible imputations from NUTS2 >> NUTS3
  potentially_imputed_from_nuts_2 <- full_code_table %>%
    filter ( nuts_level_2 %in% validated_nuts_2$geo ) %>%
    rename ( geo = nuts_level_2 ) %>%
    select ( -all_of(c("nuts_level_1"))) %>%
    left_join ( mixed_nuts_example, by = 'geo' ) %>%
    mutate ( method = paste0(
      "imputed from NUTS2 ", 
      geo, " [", method, "]")
    ) %>%
    select ( all_of(c("nuts_level_3", "values", "method"))) %>%
    rename ( geo = nuts_level_3 ) %>%
    filter ( ! geo %in%  validated$geo )
  
  ## Now add possible imputations from country level NUTS0 >> NUTS3
  imputed_from_nuts_2 <- potentially_imputed_from_country %>%
    distinct_at  ( tidyselect::all_of (c("nuts_level_3", "values", 
                                         "method")))  %>%
    rename ( geo = nuts_level_3 ) %>%
    filter ( ! geo %in% potentially_imputed_from_nuts_2$geo ) %>%
    bind_rows ( potentially_imputed_from_nuts_2 )
  
  ## Now add the original data and the the NUTS2 >> NUTS3 imputations
  actual_to_imputed <- validated %>%
    full_join ( imputed_from_nuts_2,
                by = c("geo", "values", "method") ) 
  
  ## Now add whatever can be added from NUTS0 or NUTS1
  imputed_df <- imputed_from_nuts_1 %>%
    filter ( ! geo %in% actual_to_imputed$geo ) %>%
    distinct_at ( tidyselect::all_of(c("geo", "values", "method"))) %>%
    bind_rows (  actual_to_imputed ) %>%
    distinct_at ( tidyselect::all_of( c("geo", "values", "method")) ) 
  
  ## And at last, add NUTS0 >> NUTS1 imputations 
  imputed_df_2 <- potentially_imputed_from_country %>%
    distinct_at ( tidyselect::all_of(c("nuts_level_1", "values", "method"))) %>%
    dplyr::rename ( geo = nuts_level_1 ) %>%
    bind_rows ( imputed_df )
  
  imputed_dfv <- validate_nuts_regions(imputed_df_2,
                                       nuts_year = nuts_year ) %>%
    dplyr::arrange(geo)
  
  ## Now only valids are returned.  Maybe all should be.
  imputed_dfv
}















