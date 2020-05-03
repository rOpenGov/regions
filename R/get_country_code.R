#' Get Country Code Of Regions
#'
#' The function identifies the sub-national geographical identifiers from 
#' known typologies and returns the ISO 3166-1 alpha-2 country codes.
#' @param dat A data frame with a 2-character geo variable to be validated
#' @param typology Currently the following typologies are supported:
#' \code{"NUTS1"}, \code{"NUTS2"}, \code{"NUTS3"} or \code{"NUTS"} for any of
#' the NUTS typologies. The technical typology \code{"NUTS0"} 
#' can be used to translate Eurostat country codes to ISO 3166-1 alpha-2 
#' country codes.
#' @return The ISO 3166-1 alpha-2 codes of the countries.
#' @family recode functions
#' @examples{
#' get_country_code (c("EL", "GR", "DED", "HU102"))
#' }
#' @export

get_country_code <- function(geo, typology = "NUTS") {
  
  if ( ! typology %in% c("NUTS", "NUTS1", "NUTS2", "NUTS3", "NUTS0")) {
    stop("Currently only NUTS-like typologies are supported.")
  }
  
  df <- data.frame( 
    geo  = as.character(geo), 
    code = substr(geo, 1,2),
    stringsAsFactors = FALSE)  %>%
    validate_nuts_country(., 
                          geo_var = "code") %>%
    mutate ( code = case_when ( 
      code == 'EL' ~ "GR", 
      code == "UK" ~ "GB", 
      TRUE ~ code ))
  
  as.character(df$code)
  
}
