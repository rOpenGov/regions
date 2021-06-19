#' Validate Conformity With NUTS Geo Codes
#'
#' Validate that \code{geo_var} is conforming with the \code{NUTS1},
#' \code{NUTS2}, or \code{NUTS3} typologies.
#' While country codes are technically not part of the NUTS typologies,
#' Eurostat de facto uses a \code{NUTS0} typology to identify countries.
#' This de facto typology has three exception which are handled by the
#' \link[regions]{validate_nuts_countries} function.
#'
#' NUTS typologies have different versions, therefore the conformity
#' is validated with one specific versions, which can be any of these:
#' \code{1999}, \code{2003}, \code{2006}, \code{2010},
#'  \code{2013}, the currently used \code{2016} and the already
#'  announced and defined \code{2021}.
#'
#' The NUTS typology was codified with the \code{NUTS2003}, and the
#' pre-1999 NUTS typologies may confuse programmatic data processing,
#' given that some  NUTS1 regions were identified with country codes
#' in smaller countries that had no \code{NUTS1} divisions.
#'
#' Currently the \code{2016} is used by Eurostat, but many datasets
#' still contain \code{2013} and sometimes earlier metadata.
#'
#' @param dat A data frame with a 3-5 character \code{geo_var}
#' variable to be validated.
#' @param geo_var Defaults to \code{"geo"}. The variable that contains
#' the 3-5 character geo codes to be validated.
#' @param nuts_year The year of the NUTS typology to use.
#' Defaults to \code{2016}.  You can select any valid
#' NUTS definition, i.e. \code{1999}, \code{2003}, \code{2006},
#' \code{2010}, \code{2013}, the currently used \code{2016} and the
#' already announced and defined \code{2021}.
#' @importFrom dplyr mutate select mutate_if left_join distinct_all
#' @importFrom dplyr bind_cols
#' @importFrom tidyselect starts_with all_of
#' @importFrom purrr set_names
#' @importFrom utils data
#' @importFrom stringr str_sub
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#' @family validate functions
#' @return Returns the original \code{dat} data frame with a column
#' that specifies the comformity with the NUTS definition of the year
#' \code{nuts_year}.
#' @examples
#' \donttest{
#' my_reg_data <- data.frame (
#'   geo = c("BE1", "HU102", "FR1",
#'           "DED", "FR7", "TR", "DED2",
#'           "EL", "XK", "GB"),
#'   values = runif(10))
#'
#' validate_nuts_regions (my_reg_data)
#'
#' validate_nuts_regions (my_reg_data, nuts_year = 2013)
#'
#' validate_nuts_regions (my_reg_data, nuts_year = 2003)
#' }
#' @export

validate_nuts_regions <- function (dat,
                                   geo_var = "geo",
                                   nuts_year = 2016) {
  
  ## These will be loaded into the environment of the function----------------------
  all_valid_nuts_codes <- nuts_exceptions <- NULL
  
  ## validate parameters -----------------------------------------------------------
  validate_data_frame (dat = dat, 
                       geo_var = geo_var, 
                       nuts_year = nuts_year)
  
  original_names <- names (dat)
  names_changed <- FALSE
  
  if (any(
    c("typology", "nuts",  paste0("valid_", as.character(nuts_year))
      ) %in% original_names)
    ) {
    temporary_names <- paste0("orig_", original_names)
    geo_var <- paste0("orig_", geo_var)
    names(dat) <- temporary_names
    names_changed <- TRUE
  }
  
  ## Loading correspondence tables ------------------------------------------------------
  
  utils::data (all_valid_nuts_codes,
               package = "regions",
               envir = environment())
  
  utils::data (nuts_exceptions,
               package = "regions",
               envir = environment())
  
  ## Validation starts here  ------------------------------------------------------------
  names(nuts_exceptions)[1] <- geo_var
  names(nuts_exceptions)[2] <- "exception"
  
  filtering <- grepl(as.character(nuts_year),
                     all_valid_nuts_codes$nuts)
  
  replace_names <- c(original_names,
                     "typology",
                     "nuts",
                     paste0("valid_", nuts_year))
  
  filtered_nuts_data_frame <- all_valid_nuts_codes[filtering,]
  names(filtered_nuts_data_frame)[2] <- geo_var
  
  return_df <- validate_nuts_countries(dat = dat,
                                       geo_var = geo_var) %>%
    dplyr::rename (
      ## Typology exceptions
      typology2 = .data$typology
      ) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    left_join (filtered_nuts_data_frame,
               by = geo_var) %>%
    mutate (
      ## Try exceptions if not found in original
      typology = ifelse (is.na(.data$typology),
                         .data$typology2,
                         .data$typology)
    ) %>%
    mutate (
      # make exception for country codes, which are anyway not
      # part of NUTS and may be valid codes
      nuts = ifelse(
        test = is.na(.data$nuts) & .data$typology == "country",
        yes  = unique(.data$nuts[which(!is.na(unique(.data$nuts)))]),
        no = .data$nuts
      )) %>%  ## countries may not be EU countries
    left_join (# join non-EU valid codes
      nuts_exceptions, 
      by = c(geo_var)) %>%
    mutate (nuts = ifelse(is.na(.data$nuts), 
                          yes = .data$exception, 
                          no  = .data$nuts)) %>%
    mutate (typology = ifelse(is.na(.data$typology), 
                              yes = .data$exception, 
                              no  = .data$typology)
            ) %>%
    mutate (valid =  !is.na(.data$nuts))  %>%
    select (-all_of(c("typology2", "exception", "nuts")))
  
  names(return_df)[which(names(return_df) == 'valid')] <-
    paste0("valid_", nuts_year)
  
  if (names_changed) {
    potentially_change_back <-
      temporary_names[names(return_df) %in% temporary_names]
    
    change_back <- potentially_change_back [
      !potentially_change_back %in% c('orig_typology', 
                                      paste0("valid_", as.character(nuts_year)))
      ]
    
    new_names <- ifelse (
      test = names(return_df) %in% change_back,
      yes  = stringr::str_sub(names(return_df), 6,-1),
      no   = names(return_df)
    )
    
    names (return_df) <- new_names
  }
  
  return_df
}
