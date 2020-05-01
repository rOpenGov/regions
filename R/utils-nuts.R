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


#' Validate comformity with NUTS geo codes
#'
#' @param dat A data frame with a 3-5 character geo variable to be validated
#' @param geo_var Defaults to \code{"geo"}. The variable that contains the
#' 3-5 character geo codes to be validated.
#' @importFrom dplyr mutate select mutate_if left_join distinct_all
#' @importFrom tidyselect one_of
#' @importFrom purrr set_names
#' @importFrom utils data 
#' @examples{
#' my_reg_data <- data.frame ( 
#'   geo = c("BE1", "HU102", "FR1", 
#'           "DED", "FR7", "TR", "DED2",
#'           "EL", "XK", "GB"), 
#'   values = runif(10))
#' 
#' validate_regional_df ( my_reg_data )
#' }

validate_nuts_regions <- function ( dat, 
                                   geo_var = "geo",
                                   nuts_year = 2016 ) {
  
  original_names <- names (dat)

  utils::data (all_valid_nuts_codes, package ="regions", 
               envir = environment())
  
  filtering <-  grepl( as.character( nuts_year), 
                       all_valid_nuts_codes$nuts )
 
  validate_nuts_country(dat) %>%
    dplyr::rename ( typology2 = typology ) %>%
    dplyr::rename ( geo = !! geo_var )  %>%
    mutate_if(is.factor, as.character) %>% 
    left_join (  all_valid_nuts_codes[filtering,],
                by = geo_var ) %>%
    mutate ( typology = ifelse (is.na(typology), 
                                typology2, 
                                typology)) %>%
    mutate ( nuts = ifelse(is.na(nuts)& typology == "country",
                           yes  = unique(nuts[which(!is.na(unique(nuts)))]),
                           no = nuts)) %>%
    select ( -one_of("typology2")) %>%
    mutate ( valid =  !is.na(nuts)) %>%
    purrr::set_names ( c(original_names, "typology", "nuts",
                         paste0("valid_", nuts_year) )) %>%
    dplyr::select ( -one_of("nuts") ) %>%
    dplyr::distinct_all ()
  
}
