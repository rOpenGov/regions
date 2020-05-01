#' Validate comformity with NUTS country codes
#'
#' @param dat A data frame with a 2-character geo variable to be validated
#' @param geo_var Defaults to \code{"geo"}. The variable that contains the
#' 2 character geo codes to be validated.
#' @importFrom dplyr mutate rename select mutate_if case_when
#' @importFrom tidyselect one_of
#' @importFrom countrycode countrycode
#' @examples{
#' my_dat <- data.frame ( 
#'  geo = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ), 
#'  values = runif(8)
#'  )
#'  validate_nuts_country(my_dat)
#' }
#' 

validate_nuts_country <- function ( dat, geo_var = "geo") {
  
  dat <-  mutate_if ( dat, is.factor, as.character )
  if (! "typology" %in% names(dat) ) {
    dat$typology <- NA_character_
  }
  
  original_names <- names(dat)
  
  validate_country_df <- dat %>%
    select ( one_of ( original_names )) %>%
    rename ( !! geo_var := geo ) %>%
    mutate ( iso2c = geo ) %>%
    mutate ( iso2c = dplyr::case_when ( 
      iso2c == "UK" ~ "GB",
      iso2c == "EL" ~ "GR", 
      iso2c == "XK" ~ "RS", #only to avoid warning
      TRUE ~ iso2c)) %>%
    mutate ( iso3c = countrycode::countrycode(iso2c, "iso2c", "iso3c")) %>%
    mutate ( typology = dplyr::case_when ( 
      is.na(iso3c)  & nchar(geo) == 2 ~ "invalid_country_code" ,
      !is.na(iso3c) & nchar(geo) == 2 ~ "country", 
      is.na(iso3c)  & nchar(geo) != 2  ~ typology 
    ))
  
  validate_country_df %>%
    select ( one_of(original_names) )
}