#' Validate Comformity With NUTS Geo Codes
#'
#' @param dat A data frame with a 3-5 character \code{geo_var} variable
#' to be validated.
#' @param geo_var Defaults to \code{"geo"}. The variable that contains the
#' 3-5 character geo codes to be validated.
#' @importFrom dplyr mutate select mutate_if left_join distinct_all
#' @importFrom tidyselect one_of
#' @importFrom purrr set_names
#' @importFrom utils data 
#' @family validate functions
#' @examples{
#' my_reg_data <- data.frame ( 
#'   geo = c("BE1", "HU102", "FR1", 
#'           "DED", "FR7", "TR", "DED2",
#'           "EL", "XK", "GB"), 
#'   values = runif(10))
#' 
#' validate_nuts_region (my_reg_data)
#' }

validate_nuts_region <- function ( dat, 
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
