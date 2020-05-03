#' Recode Region Codes From Source To Target NUTS Typology
#' 
#' @param dat A data frame with a 3-5 character \code{geo_var} variable
#' to be validated.
#' @param geo_var Defaults to \code{"geo"}. The variable that contains 
#' the 3-5 character geo codes to be validated.
#' @param target_nuts_year Defaults to the current typology in force, which 
#' is \code{2016}.
#' @importFrom dplyr mutate select mutate_if left_join distinct vars
#' @importFrom dplyr bind_cols bind_rows ungroup group_by_at summarize
#' @importFrom dplyr rename
#' @importFrom tidyselect one_of starts_with
#' @importFrom tidyr unite pivot_longer
#' @importFrom purrr set_names
#' @importFrom stats complete.cases
#' @importFrom utils data 
#' @family recode functions
#' @examples{
#' foo <- data.frame ( 
#'   geo  =  c("FR", "DEE32", "UKI3" ,
#'             "HU12", "DED", 
#'             "FRK"), 
#'   values = runif(6, 0, 100 ),
#'   stringsAsFactors = FALSE )
#' 
#' recode_nuts(foo, target_nuts_year = 2013)
#' }
#' @export

recode_nuts <- function( dat, 
                         geo_var = "geo",
                         target_nuts_year = 2016 ) {
  
  . <- nuts <- nuts_changes <- target <- geo <- typology <- NULL
  years <- typology_change <- min_year <- max_year <- NULL
  all_valid_nuts_codes <- NULL
  
  
  target_code <- paste0("code_", target_nuts_year)
  source_code <- paste0("source_", target_nuts_year)
  
  dat <- mutate_if ( dat, is.factor, as.character )
  
  if ( geo_var != "geo" ) {
    dat <- dat %>%
      dplyr::rename ( geo = !! geo_var )
  }
 
  utils::data (all_valid_nuts_codes, package ="regions", 
               envir = environment())
  
  utils::data (nuts_changes, package ="regions", 
               envir = environment())
  
  
  codes_in_target_year <- all_valid_nuts_codes %>% 
    dplyr::filter (nuts == paste0("code_", target_nuts_year)) %>%
    dplyr::filter (!is.na(geo)) %>%
    select ( -nuts ) %>%
    distinct ( typology, geo ) %>%
    mutate ( geo2 = geo ) %>%
    purrr::set_names ( c("typology", target_code,
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
  
  select_from_correspondence <- unique(c("typology", 
                                         possible_codes$nuts, 
                                         target_code))
  
  recoding_changes <- nuts_changes  %>%
    select ( tidyselect::one_of(select_from_correspondence ) ) %>%
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
