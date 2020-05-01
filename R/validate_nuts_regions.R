#' Validate Comformity With NUTS Geo Codes
#' 
#' Validate that \code{geo_var} is conforming with the \code{NUTS1}, 
#' \code{NUTS2}, or \code{NUTS3} typologies.
#' While country codes are technically not part of the NUTS typologies, 
#' Eurostat de facto uses a \code{NUTS0} typology to identify countries.
#' This de facto typology has three exception which are handled by the 
#' \link[regions]{validate_nuts_country} function.
#' 
#' NUTS typologies have different versions, therefore the comformity is 
#' validated with one specific verions, which can be any of these:
#' \code{1999}, \code{2003}, \code{2006}, \code{2010},
#'  \code{2013}, the currently used \code{2016} and the already 
#'  announced and defined \code{2021}. 
#'   
#' The NUTS typology was codified with the \code{NUTS2003}, and the
#' pre-1999 NUTS typologies may confuse programmatic data processing, 
#' given that some  NUTS1 regions were identified with country codes
#' in smaller countries that had no \code{NUTS1} divisions.
#' 
#' Currently the \code{2016} is used by Eurostat, but many datasets still 
#' contain  \code{2013} and sometimes earlier metadata. 
#' @param dat A data frame with a 3-5 character \code{geo_var} variable
#' to be validated.
#' @param geo_var Defaults to \code{"geo"}. The variable that contains the
#' 3-5 character geo codes to be validated.
#' @param nuts_year Defaults to \code{2016}.  You can select any valid 
#' NUTS definition, i.e. \code{1999}, \code{2003}, \code{2006}, \code{2010},
#'  \code{2013}, the currently used \code{2016} and the already 
#'  announced and defined \code{2021}.
#' @importFrom dplyr mutate select mutate_if left_join distinct_all
#' @importFrom tidyselect one_of
#' @importFrom purrr set_names
#' @importFrom utils data 
#' @family validate functions
#' @return Returns the orginal \code{dat} data frame with a column that 
#' specifies the comformity with the NUTS definition of the year 
#' \code{nuts_year}. 
#' @examples{
#' my_reg_data <- data.frame ( 
#'   geo = c("BE1", "HU102", "FR1", 
#'           "DED", "FR7", "TR", "DED2",
#'           "EL", "XK", "GB"), 
#'   values = runif(10))
#' 
#' validate_nuts_region (my_reg_data)
#' 
#' validate_nuts_region (my_reg_data, nuts_year = 2013)
#' 
#' validate_nuts_region (my_reg_data, nuts_year = 2003)
#' }

validate_nuts_region <- function ( dat, 
                                   geo_var = "geo",
                                   nuts_year = 2016 ) {
  
  if ( ! nuts_year %in% c(1999,2003,2006,2010,2013,2016,2021)) {
    stop('"nuts_year" = ', nuts_year, " is an invalid parameter."  )
  }
  
  original_names <- names (dat)

  utils::data (all_valid_nuts_codes, package ="regions", 
               envir = environment())
  
  filtering <-  grepl( as.character( nuts_year), 
                       all_valid_nuts_codes$nuts )
 
  validate_nuts_country(dat) %>%
    dplyr::rename ( typology2 = typology ) %>%
    dplyr::rename ( geo = !! geo_var )  %>%
    mutate_if(is.factor, as.character) %>% 
    left_join (  all_valid_nuts_codes[filtering, ],
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
