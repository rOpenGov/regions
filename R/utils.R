#' Validate Parameter 'dat'
#' @param dat A data frame input to be validated.

validate_data_frame <- function(dat) {
  
  if ( ! "data.frame" %in% class(dat) ) {
    stop ("The parameter 'dat' must be a data.frame-like object, like a data frame or a tibble.")
  } 
  
  if ( nrow(dat) == 0 ) {
    stop ( "The data frame has no observations.")
  }
  
}
#' Create the nuts_lau_2019 correspondence table
#' May be used to create similar historical correspondence tables. 
#' @importFrom dplyr across mutate case_when
#' @importFrom readxl read_excel
#' @importFrom purrr set_names
#' @importFrom tidyselect contains
#' @return A data.frame which is also saved and can be retrieved with
#' \code{data(nuts_lau_2019).} Use this function as a template to 
#' obtain historical correspondence tables.

create_nuts_lau_2019 <- function() {
  eu_lau_2019_link <- "https://ec.europa.eu/eurostat/documents/345175/501971/EU-28-LAU-2019-NUTS-2016.xlsx"
  
  nuts_countries <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "EL", 
                      "ES", "FR", "IT", "CY", "LV", "LT", "LU", "HU", 
                      "MT", "NL", "AT", "PL", "PT", "RO", 
                      "SI", "SK", "FI", "SE", "UK", "IS", "LI", 
                      "CH",  "MK", "AL", "TR")
  
  table_names <- c("code_2016", "lau_code", "lau_name_national",
                   "lau_name_latin", "name_change_last_year", 
                   "population", "total_area_m2", 
                   "degurba", "degurba_change_last_year", 
                   "coastal_area", 
                   "coastal_change_last_year",
                   "city_id", "city_id_change_last_year", 
                   "city_name", "greater_city_id", 
                   "greater_city_id_change_last_year", 
                   "greater_city_name",
                   "fua_id", "fua_id_change_last_year", "fua_name", 
                   "country", "gisco_id")
  
  table_names
  
  tempxl <- tempfile()
  download.file(eu_lau_2019_link, tempxl, mode="wb")
  eu_lau_raw <- readxl::read_excel(path = tempxl, sheet = "Combined")
  
  x <- c("y", "no", "n")
  change_to_logical <- function(x) {
    x <- tolower(x)
    dplyr::case_when ( substr(x, 1,1) == "n" ~ FALSE, 
                       substr(x, 1,1) == "y" ~ TRUE,
                       TRUE ~ NA )
  }
  
  names ( eu_lau_raw )
  eu_lau_2019 <- eu_lau_raw %>%
    purrr::set_names(., table_names ) 
  
  nuts_lau_2019 <- eu_lau_2019 %>%
    dplyr::mutate ( 
      dplyr::across(
        tidyselect::contains("change_last_year"), change_to_logical) 
    )
  
  nuts_lau_2019
}