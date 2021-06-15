#' Get Country Code Of Regions
#'
#' The function identifies the sub-national geographical identifiers from
#' known typologies and returns the ISO 3166-1 alpha-2 country codes.
#' @param geo A character variable with geo codes.
#' @param typology Currently the following typologies are supported:
#' \code{"NUTS1"}, \code{"NUTS2"}, \code{"NUTS3"} or \code{"NUTS"} for any of
#' the NUTS typologies. The technical typology \code{"NUTS0"}
#' can be used to translate Eurostat country codes to ISO 3166-1 alpha-2
#' country codes.
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @return The ISO 3166-1 alpha-2 codes of the countries as a character
#' vector.
#' @family recode functions
#' @examples{
#' get_country_code (c("EL", "GR", "DED", "HU102"))
#' }
#' @export

get_country_code <- function(geo, typology = "NUTS") {

  validate_parameters ( param = typology, 
                       param_name = "typology", 
                       typology = typology)
  
  df <- tibble::tibble(
    geo  = as.character(geo),
    code = substr(geo, 1, 2)
  )
  
  df <- validate_nuts_countries(dat = df, geo_var = "code") %>%
    mutate (code = case_when (.data$code == 'EL' ~ "GR",
                              .data$code == "UK" ~ "GB",
                              TRUE ~ .data$code))
  
  as.character(df$code)
  
}
