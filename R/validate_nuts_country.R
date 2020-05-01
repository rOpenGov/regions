#' Validate Comformity with NUTS Country Codes
#'
#' This function is mainly a wrapper around the well-known 
#'  \link[countrycode]{countrycode} function, with three exception that are 
#'  particular to the European Union statistical nomenclature. 
#' \describe{
#'   \item{EL}{Treated valid, because NUTS uses EL instead of GR for Greece since 2010.}
#'   \item{UK}{Treated valid, because NUTS uses UK instead of GB for the United Kingdom.}
#'   \item{XK}{XK is used for Kosovo, because Eurostat uses this code, too.}
#' }
#' All ISO-3166 country codes are validated, and the three exceptions, too.
#' @param dat A data frame with a 2-character geo variable to be validated
#' @param geo_var Defaults to \code{"geo"}. The variable that contains the
#' 2 character geo codes to be validated.
#' @importFrom dplyr mutate rename select mutate_if case_when
#' @importFrom tidyselect one_of
#' @importFrom countrycode countrycode
#' @importFrom purrr quietly
#' @return The original data frame.
#' @family validate functions
#' @examples{
#' my_dat <- data.frame ( 
#'  geo = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ), 
#'  values = runif(8)
#'  )
#'  
#'  ## NLD is an ISO 3-character code and is not validated.
#'  validate_nuts_country(my_dat)
#' }
#' @export


validate_nuts_country <- function ( dat, geo_var = "geo" ) {
  
  dat <-  mutate_if ( dat, is.factor, as.character )
  if (! "typology" %in% names(dat) ) {
    dat$typology <- NA_character_
  }
  
  original_names <- names(dat)
  
  quiet_country_codes <- purrr::quietly(countrycode::countrycode)
  
  quiet_country_codes (validate_country_df$iso2c, "iso2c", "iso3c")
  
  validate_country_df <- dat %>%
    select ( one_of ( original_names )) %>%
    rename ( !! geo_var := geo ) %>%
    mutate ( iso2c = geo ) %>%
    mutate ( iso2c = dplyr::case_when ( 
      iso2c == "UK" ~ "GB",
      iso2c == "EL" ~ "GR", 
      iso2c == "XK" ~ "GR", #only to avoid warning
      TRUE ~ iso2c)) %>%
    mutate ( iso3c = quiet_country_codes (iso2c, "iso2c", "iso3c")$result) %>%
    mutate ( typology = dplyr::case_when ( 
      is.na(iso3c)  & nchar(geo) == 2 ~ "invalid_country_code" ,
      !is.na(iso3c) & nchar(geo) == 2 ~ "country", 
      is.na(iso3c)  & nchar(geo) != 2  ~ typology 
    ))
  
  validate_country_df %>%
    select ( one_of(original_names) )
}
