#' Recode Region Codes From Source To Target NUTS Typology
#'
#' Validate your geo codes, pair them with the appropriate standard
#' typology, look up potential causes of invalidity in the EU correspondence
#' tables, and look up the appropriate geographical codes in the other
#' (target) typology.  For example, validate geo codes in the \code{'NUTS2016'}
#' typology and translate them to the now obsolete the \code{'NUTS2010'} typology
#' to join current data with historical data sets.
#' @param dat A data frame with a 3-5 character \code{geo_var} variable
#' to be validated.
#' @param geo_var Defaults to \code{"geo"}. The variable that contains
#' the 3-5 character geo codes to be validated.
#' @param nuts_year The year of the NUTS typology to use.
#' You can select any valid
#' NUTS definition, i.e. \code{1999}, \code{2003}, \code{2006},
#' \code{2010}, \code{2013}, the currently used \code{2016} and the
#' already announced and defined \code{2021}. Defaults to the current
#' typology in force, which is \code{2016}.
#' @importFrom dplyr mutate select mutate_if left_join distinct vars
#' @importFrom dplyr bind_cols bind_rows ungroup group_by_at summarize
#' @importFrom dplyr rename filter_at all_vars
#' @importFrom tidyselect starts_with all_of
#' @importFrom tidyr unite pivot_longer
#' @importFrom purrr set_names
#' @importFrom stats complete.cases
#' @importFrom utils data
#' @family recode functions
#' @return The original data frame with a \code{'geo_var'} column is extended
#' with a \code{'typology'} column that states in which typology is the \code{'geo_var'}
#' a valid code.  For invalid codes, looks up potential reasons of invalidity
#' and adds them to the \code{'typology_change'} column, and at last it
#' adds a column of character vector containing the desired codes in the
#' target typology, for example, in the NUTS2013 typology.
#' @examples{
#' foo <- data.frame (
#'   geo  =  c("FR", "DEE32", "UKI3" ,
#'             "HU12", "DED",
#'             "FRK"),
#'   values = runif(6, 0, 100 ),
#'   stringsAsFactors = FALSE )
#'
#' recode_nuts(foo, nuts_year = 2013)
#' }
#' @export

recode_nuts <- function(dat,
                        geo_var = "geo",
                        nuts_year = 2016) {
  if (!"data.frame" %in% class(dat)) {
    stop ("The input variable 'dat' must be a data.frame (like) object.")
  }
  
  if (nrow(dat) < 1) {
    stop ("The input variable 'dat' must have at least one row (observation)")
  }
  
  . <- nuts <- nuts_changes <- target <- geo <- typology <- NULL
  years <- typology_change <- min_year <- max_year <- NULL
  
  original_geo_codes <- as.character(unlist(dat[, geo_var]))
  
  target_code <- paste0("code_", nuts_year)
  source_code <- paste0("source_", nuts_year)
  
  dat <- mutate_if (dat, is.factor, as.character)
  
  utils::data (all_valid_nuts_codes,
               package = "regions",
               envir = environment())
  
  all_valid_nuts_codes <- all_valid_nuts_codes
  
  utils::data (nuts_changes, package = "regions",
               envir = environment())
  
  codes_in_target_year <- all_valid_nuts_codes %>%
    dplyr::filter (nuts == paste0("code_", nuts_year)) %>%
    dplyr::filter (!is.na(geo)) %>%
    select (-tidyselect::all_of("nuts")) %>%
    distinct (typology, geo) %>%
    mutate (geo2 = geo) %>%
    purrr::set_names (c("typology", target_code,
                        "geo"))
  
  names(codes_in_target_year)[which(names(codes_in_target_year) == "geo")] <-
    geo_var
  names(all_valid_nuts_codes)[which(names(all_valid_nuts_codes) == "geo")] <-
    geo_var
  
  join_by_vars <- names(codes_in_target_year)[names(codes_in_target_year) %in% names(dat)]
  
  ## Valid geo codes in the targeted year ---------------------------
  target_geo_codes <- codes_in_target_year[,
                                           which(names(codes_in_target_year) == target_code)]
  target_geo_codes <- as.character(unlist(target_geo_codes))
  
  target_coding <- dat %>%
    dplyr::filter_at(vars(all_of(geo_var)),
                     all_vars(. %in% target_geo_codes))  %>%
    left_join (all_valid_nuts_codes, by = join_by_vars) %>%
    dplyr::filter (nuts == paste0("code_", nuts_year)) %>%
    mutate (typology_change = NA_character_) %>%
    select (-all_of("nuts")) %>%
    mutate (target = as.character(unlist(.[, geo_var])))
  
  names(target_coding)[which(names(target_coding) == "target")] <-
    target_code
  
  correct_geo_codes <-
    as.character(unlist(target_coding[, geo_var]))
  
  return_values <- target_coding
  
  ## These are differently coded ------------------------------------
  different_coding <- dat %>%
    dplyr::filter_at(vars(all_of(geo_var)),
                     all_vars(!. %in% target_geo_codes))
  
  different_codes <-
    as.character(unlist (different_coding[, geo_var]))
  
  valid_different_codes <-  all_valid_nuts_codes %>%
    dplyr::filter_at(vars(tidyselect::all_of(geo_var)),
                     dplyr::all_vars(. %in% different_codes)) %>%
    mutate (years = as.numeric(gsub("code_", "" , nuts)))
  
  ## Find the valid different codes in correspondence -------------
  
  select_from_correspondence <- unique(c("typology",
                                         valid_different_codes$nuts,
                                         target_code))
  
  recoding_changes <- nuts_changes
  
  if (length(valid_different_codes$nuts) > 0) {
    recoding_changes <- recoding_changes   %>%
      select (tidyselect::all_of(select_from_correspondence)) %>%
      rename (target = !!target_code)
  }
  
  anything_to_fold <- any(grepl("code_",
                                substr(names(recoding_changes), 1, 5)))
  
  if (anything_to_fold) {
    recoding_changes <- tidyr::pivot_longer (
      recoding_changes,
      cols =  c(starts_with('code')),
      names_to  = 'nuts',
      values_to = 'geo'
    )
  }
  
  recoding_changes <- recoding_changes %>%
    dplyr::filter (geo %in% different_codes)  %>%
    dplyr::filter (!is.na(target)) %>%
    mutate (years = as.numeric(gsub("code_", "", nuts)))
  
  if (nrow(recoding_changes) > 0) {
    recoding_changes <-  recoding_changes %>%
      group_by_at (dplyr::vars(-all_of(c("years", "nuts")))) %>%
      summarize (
        min_year = min(years, na.rm = TRUE),
        max_year = max(years, na.rm = TRUE),
      ) %>%
      tidyr::unite (typology_change,
                    min_year, max_year, sep =   '-') %>%
      mutate (typology_change = paste0("Recoded from ",
                                       geo, " [used in NUTS ",
                                       typology_change, "]"))  %>%
      ungroup()
    
    names(recoding_changes)[which(names(recoding_changes) == "target")] <-
      target_code
    names(recoding_changes)[which(names(recoding_changes) == "geo")] <-
      geo_var
    
    
    original_names_in_recoding_changes <-
      names(recoding_changes)[names (recoding_changes) %in% names(dat)]
    
    ## Recode those that can be recoded with a valid code -------------------
    
    recoded_values <- dat %>%
      left_join (recoding_changes,
                 by = original_names_in_recoding_changes) %>%
      dplyr::filter (!is.na(typology_change))
    
    names(recoded_values)[which(names(recoded_values) == "target")] <-
      target_code
    
    recoded_geo_codes <-
      as.character(unlist(recoded_values[, geo_var]))
    
    return_values <- return_values %>%
      dplyr::bind_rows (recoded_values)
    
    ## Add those that are valid but cannot be recoded ---------
    valid_but_not_recoded <- valid_different_codes  %>%
      mutate (years  = as.numeric(gsub("code_", "", nuts))) %>%
      filter_at(vars(all_of(geo_var)),
                all_vars(!. %in% c(
                  recoded_geo_codes,
                  correct_geo_codes
                )))
    
    if (nrow(valid_but_not_recoded) > 0) {
      valid_but_not_recoded <- valid_but_not_recoded  %>%
        group_by_at (vars(-all_of(c(
          "years", "nuts"
        )))) %>%
        summarize (
          ## find earliest and latest mention of the code
          min_year = min(years, na.rm = TRUE),
          max_year = max(years, na.rm = TRUE)
        ) %>%
        tidyr::unite (typology_change, min_year, max_year, sep =   '-') %>%
        mutate (typology_change = paste0("Used in NUTS ", typology_change))  %>%
        ungroup()
      
      differently_coded <- valid_but_not_recoded %>%
        left_join (dat, by = geo_var) %>%
        mutate (target = NA_character_)
      
      names(differently_coded)[which(names(differently_coded) == "target")] <-
        target_code
      
      not_recoded_geo_codes <-
        as.character(unlist(differently_coded[, geo_var]))
      
      return_values <- dplyr::bind_rows (return_values,
                                         differently_coded)
    }
  }
  
  returned_geo_codes <-
    as.character(unlist(return_values[, geo_var]))
  
  invalid_and_not_recoded <-
    original_geo_codes[!original_geo_codes %in% returned_geo_codes]
  
  if (length(invalid_and_not_recoded) == 0) {
    return_values
  } else {
    ### There are invalid codes that need to be added back -------
    invalid_not_recoded <- dat %>%
      filter_at (vars(all_of(geo_var)),
                 all_vars(. %in% invalid_and_not_recoded)) %>%
      mutate (typology_change = "Not found in NUTS",
              typology  = "invalid_typology",
              target = NA_character_)
    
    names(invalid_not_recoded)[which(names(invalid_not_recoded) == "target")] <-
      target_code
    
    return_values %>% bind_rows(invalid_not_recoded)
  }
}
