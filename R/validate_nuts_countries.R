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
#' @importFrom dplyr mutate rename select mutate_if case_when left_join
#' @importFrom tidyselect all_of
#' @importFrom countrycode countrycode
#' @importFrom purrr quietly
#' @importFrom magrittr %>%
#' @return The original data frame extended with the column \code{'typology'}. 
#' This column states \code{'country'} for valid country typology coding, or
#' appropriate label for invalid ISO-3166-alpha-2 and ISO-3166-alpha-3 codes.
#' @family validate functions
#' @examples{
#' my_dat <- data.frame ( 
#'  geo = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ), 
#'  values = runif(8)
#'  )
#'  
#'  ## NLD is an ISO 3-character code and is not validated.
#'  validate_nuts_countries(my_dat)
#' }
#' @export


validate_nuts_countries <- function ( dat, geo_var = "geo" ) {
  
  . <- geo <- geo_tmp <- iso2c <- use_geo_tmp <- NULL
  
  validate_data_frame(dat)

  dat <-  dplyr::mutate_if ( dat, is.factor, as.character )
  if (! "typology" %in% names(dat) ) {
    dat$typology <- NA_character_
  }
  
  original_names <- names(dat)
  
  quiet_country_codes <- purrr::quietly(countrycode::countrycode)

  validate_country_df <- dat %>%
    dplyr::select ( all_of ( original_names )) %>%
    dplyr::mutate ( iso2c = unlist(.[,geo_var]) ) %>%
    dplyr::mutate ( iso2c = dplyr::case_when ( 
      iso2c == "UK" ~ "GB",
      iso2c == "EL" ~ "GR", 
      iso2c == "XK" ~ "GR", #only to avoid warning
      TRUE ~ iso2c)) %>%
    dplyr::mutate ( iso3c = quiet_country_codes (iso2c, 
                                                 "iso2c", "iso3c")$result) %>%
    dplyr::mutate ( typology = dplyr::case_when ( 
      is.na (iso3c) & nchar(unlist(.[,geo_var])) == 2 ~ "invalid_iso-3166-alpha-2",
      is.na (iso3c) & nchar(unlist(.[,geo_var])) == 3 ~ "iso-3166-alpha-3",
      !is.na(iso3c) & nchar(unlist(.[,geo_var])) == 2 ~ "country", 
      is.na (iso3c) & nchar(unlist(.[,geo_var])) != 2  ~ typology 
    ))

  validate_country_df %>%
    dplyr::select ( all_of(original_names) )
}
