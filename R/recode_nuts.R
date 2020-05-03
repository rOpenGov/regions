#' Recode Region Codes From Source To Target NUTS Typology
#' 
#' @param dat A data frame with a 3-5 character \code{geo_var} variable
#' to be validated.
#' @param geo_var Defaults to \code{"geo"}. The variable that contains 
#' the 3-5 character geo codes to be validated.
#' @param source_nuts_year Defaults to 2016
#' @param target_nuts_year Defaults to 2013
#' @importFrom dplyr mutate select mutate_if left_join distinct vars
#' @importFrom dplyr bind_cols bind_rows ungroup group_by_at summarize
#' @importFrom tidyselect one_of starts_with
#' @importFrom tidyr unite pivot_longer
#' @importFrom purrr set_names
#' @importFrom stats complete.cases
#' @importFrom utils data 
#' @family recode functions
#' @examples{
#' recode_nuts ( data = data.frame ( 
#'                      geo  =  c("FR", "DEE32", "UKI3",
#'                                "HU12", "DED", 
#'                                "FRK", "FR7"), 
#'                      values = runif(7, 0, 100 ),
#'                      stringsAsFactors = FALSE), 
#'               source_nuts_year = 2016, 
#'               target_nuts_year =2013)
#' }
#' @export

recode_nuts <- function( dat, 
                         geo_var = "geo",
                         source_nuts_year = 2016,
                         target_nuts_year = 2013 ) {
  
  . <- nuts <- nut_changes <- target <- geo <- typology <- NULL
  years <- typology_change <- min_years <- max_years <- NULL
  
  target_code <- paste0("code_", target_nuts_year)
  source_code <- paste0("source_", target_nuts_year)
  
  dat <- mutate_if ( dat, is.factor, as.character )
  
  if ( geo_var != "geo" ) {
    dat <- dat %>%
      rename ( geo = !! geo_var )
  }
 
  utils::data (all_valid_nuts_codes, package ="regions", 
               envir = environment())
  
  codes_in_target_year <- all_valid_nuts_codes %>% 
    dplyr::filter (nuts == paste0("code_", target_nuts_year)) %>%
    dplyr::filter (!is.na(geo)) %>%
    select ( -nuts ) %>%
    distinct ( typology, geo ) %>%
    mutate ( geo2 = geo ) %>%
    purrr::set_names ( c("typology", target_code,
                         "geo")) 
  
  codes_in_source_year <- all_valid_nuts_codes %>% 
    dplyr::filter (nuts == paste0("code_", source_nuts_year)) %>%
    dplyr::filter (!is.na(geo)) %>%
    select ( -nuts ) %>%
    distinct ( typology, geo ) %>%
    mutate ( geo2 = geo ) %>%
    purrr::set_names ( c("typology", source_code,
                         "geo")) 
  
  join_by_vars <- names(all_valid_nuts_codes)[
    names(all_valid_nuts_codes) %in% names(dat) ]
  
  if( length(join_by_vars)==0 ) {
    stop ("Cannot join with valid nuts codes, 'geo' is not present.")
  }
  
  target_geo_codes <- codes_in_target_year[, 
       which(names(codes_in_target_year) == target_code)]
  
  target_coding <- dat %>%
    dplyr::filter ( geo %in% as.character(unlist(target_geo_codes)) ) %>%
    left_join ( all_valid_nuts_codes, by = join_by_vars ) %>%
    dplyr::filter ( nuts == paste0("code_", target_nuts_year)) %>%
    mutate ( typology_change = NA_character_ ) %>%
    select ( -one_of("nuts")) %>%
    mutate ( geo2 = geo )
  
  names(target_coding)[which(names(target_coding)=="geo2")] <- target_code
  
  different_coding <- dat %>%
    dplyr::filter ( ! geo %in% target_coding$geo ) 
  
  possible_codes <-  all_valid_nuts_codes %>%
    dplyr::filter ( geo %in% different_coding$geo ) %>%
    mutate ( years = as.numeric(gsub("code_", "" , nuts )) ) 
  
  recoding_changes <- nuts_changes  %>%
    select ( one_of("typology", 
                     possible_codes$nuts, 
                     target_code)
             ) %>%
    mutate ( target = unlist(.[, target_code ]) ) %>%
    select ( -one_of(target_code)) %>%
    tidyr::pivot_longer (., cols =  starts_with('code'), 
                  names_to = 'nuts', 
                  values_to = 'geo')  %>%
    dplyr::filter ( geo %in% different_coding$geo ) %>%
    dplyr::filter ( !is.na(target)) %>%
    mutate ( years  = as.numeric(gsub("code_", "", nuts))) %>%
    group_by_at ( dplyr::vars(-one_of("years", "nuts"))) %>%
    summarize ( min_year = min(years, na.rm=TRUE), 
                max_year = max(years, na.rm=TRUE)) %>%
    tidyr::unite ( typology_change, min_year, max_year, sep =   '-') %>%
    mutate ( typology_change = paste0("Recoded from ", 
                                      geo, " [used in NUTS ",
                                      typology_change, "]"))  %>%
    ungroup() 
  
  names(recoding_changes)[which(names(recoding_changes)=="target")] <- target_code
  
  recoded_values <- dat %>%
    left_join ( recoding_changes, 
               by = names(recoding_changes)[names (recoding_changes) %in% names(dat)] ) %>%
    dplyr::filter ( !is.na(typology_change))
  
  different_coding_typology <- possible_codes %>%
    mutate ( years  = as.numeric(gsub("code_", "", nuts))) %>%
    group_by_at ( vars(-one_of("years", "nuts"))) %>%
    summarize ( min_year = min(years, na.rm=TRUE), 
                max_year = max(years, na.rm=TRUE)) %>%
    tidyr::unite ( typology_change, min_year, max_year, sep =   '-') %>%
    mutate ( typology_change = paste0("Used in NUTS ", typology_change))  %>%
    ungroup() %>%
    dplyr::filter ( ! geo %in% recoded_values$geo )
  
  differently_coded <- different_coding_typology %>%
    left_join ( dat, by = geo_var ) %>%
    mutate ( geo2 = NA_real_ ) 
  
  names(differently_coded)[
    which(names(differently_coded)=="geo2")] <- target_code
 
  return_values <- dplyr::bind_rows ( target_coding, 
                                      recoded_values, 
                                      differently_coded )
  
  all( dat$geo %in% return_values$geo )
  
  names(return_values)[which(names(return_values)=="geo")] <- geo_var

  return_values   
}
