#' Imputing Data From Larger To Smaller Units in the EU NUTS
#' 
#' This is a special case of \code{\link{impute_down}} for the EU NUTS
#' hierarchical typologies. All valid actual rows will be projected down 
#' to all smaller constituent typologies where data is missing. 
#' 
#' The more general function requires typology information from the higher
#' and lower level typologies.  This is not needed when the EU vocabulary
#' is used, and the hierarchy can be established from the EU vocabularies.
#' 
#' Be mindful that while all possible imputations are made, imputations 
#' beyond one hiearchical level are very crude estimates.  
#' 
#' The imputed dataset \code{dat} must refer to a single time unit, i.e. 
#' panel data is not supported.
#' @param dat A data frame with exactly two or three columns: \code{geo} 
#' for the geo codes of the units, \code{values} for the values, and 
#' optionally \code{method} for describing the data source.
#' @param values_var The variable that contains the upstream data to be
#' imputed to the downstream data, defaults to \code{"values"}.
#' @param geo_var The variable that contains the geographical codes in the
#' NUTS typologies, defaults to code{"geo_var".}
#' @param method_var The variable that contains the metadata on various
#' processing information, defaults to \code{NULL} in which case it will 
#' be returned as \code{'method'}. 
#' @param nuts_year The year of the NUTS typology to use, it defaults to the 
#' currently valid \code{2016}.  Alternative values can be any of these:
#' \code{1999}, \code{2003}, \code{2006}, \code{2010},
#'  \code{2013} and the already 
#'  announced and defined \code{2021}. For example, use \code{2013} for 
#' \code{NUTS2013} data.
#' @return An augmented version of the \code{dat} imputed data frame with all
#' possible projections to valid smaller units, i.e. \code{NUTS0 = country} values 
#' imputed to all missing \code{NUTS1} units, \code{NUTS1} values 
#' imputed to all missing \code{NUTS2} units, \code{NUTS2} values 
#' imputed to all missing \code{NUTS3} units. 
#' @family impute functions
#' @importFrom dplyr mutate select distinct distinct_at filter bind_rows
#' @importFrom dplyr rename arrange
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#' @examples{
#' data(mixed_nuts_example)
#' impute_down_nuts(mixed_nuts_example, nuts_year = 2016)
#' } 
#' @export 

impute_down_nuts <- function (dat, 
                              geo_var = "geo", 
                              values_var = "values", 
                              method_var = NULL, 
                              nuts_year = 2016 ) {
 
   . <- geo <- country_code <- NULL
  
  if (! geo_var %in% names(dat) ) {
    stop(geo_var, " is not among the columns of the data frame.")
  }
  
  if (! values_var %in% names(dat) ) {
    stop(values_var, " is not among the columns of the data frame.")
  }
  
  if ( ! nuts_year %in% c(1999,2003,2006,2010,2013,2016,2021)) {
    stop('"nuts_year" = ', nuts_year, " is an invalid parameter."  )
  }
  
  if ( is.null(method_var)) {
    dat$method <- ""
    method_var <- 'method'
  }
  
  if ( ! method_var %in% names(dat)) {
    stop(method_var, " is not among the columns of the data frame.")
  }
  
  validated <- dat %>%
    rename ( ## will be turned back on return, easier to handle
             ## non-programatic geo, values names. 
             geo = geo_var, 
             values = values_var ) %>%
    validate_nuts_regions( nuts_year = nuts_year ) 
  
  names(validated)[ which(names(validated)==paste0("valid_", nuts_year))] <- "valid"
  
  validated_nuts_1 <- validated %>%
    filter ( typology == "nuts_level_1", 
             valid    == TRUE )
  
  validated_nuts_2 <- validated %>%
    filter ( typology == "nuts_level_2", 
             valid    == TRUE )
  
  countries_present <- validated %>%
    mutate ( country_code = regions::get_country_code(geo)) %>%
    select ( country_code ) %>%
    distinct ( country_code ) %>%
    unlist() %>% as.character() %>% sort()
  
  all_valid_nuts_codes_year <- all_valid_nuts_codes %>%
    filter ( nuts     ==  paste0("code_", nuts_year) , 
             typology == "nuts_level_3") %>%
    mutate ( 
      ## start with smallest hierarchival unit, which is NUTS3
      country_code = get_country_code (geo), 
      row_id       = 1:nrow(.)
    ) %>%
    pivot_wider ( ., names_from = "typology", 
                  values_from = 'geo') %>%
    mutate ( 
      nuts_level_2 = substr(nuts_level_3, 1, 4), 
      nuts_level_1 = substr(nuts_level_3, 1, 3), 
      country      = substr(nuts_level_3, 1, 2), 
    ) %>%
    select ( -all_of("row_id") )
  
  full_code_table <- all_valid_nuts_codes_year %>%   ## all the regions of the countries present in the datatable
    filter ( country_code %in% countries_present ) 

  ## Create all possible imputations from NUTS0 >> NUTS1 >> NUTS2 >> NUTS3
  potentially_imputed_from_country <- full_code_table %>%
    filter ( country_code %in% countries_present ) %>%
    dplyr::rename ( geo  = country_code ) %>%
    left_join ( dat, by = 'geo') %>%
    filter ( !is.na(values) ) %>%
    mutate ( method = paste0(
      "imputed from country ", 
      geo, " [", method, "]")
    ) 
  
  ## Create all possible imputations from NUTS1 >> NUTS2 >> NUTS3
  potentially_imputed_from_nuts_1 <- full_code_table %>%
    filter ( nuts_level_1 %in% validated_nuts_1$geo ) %>%
    rename ( geo = nuts_level_1 ) %>%
    left_join ( dat, by = 'geo' ) %>%
    mutate ( method = paste0(
      "imputed from NUTS1 ", 
      geo, " [", method, "]")
    ) %>%
    select ( all_of(c("nuts_level_2", "values", "method"))) %>%
    dplyr::rename ( geo = nuts_level_2 )
  
  ## Now add potential imputations from NUTS0 if not present in NUTS1
  imputed_from_nuts_1 <- potentially_imputed_from_country %>%
    distinct_at  ( tidyselect::all_of (c("nuts_level_2", "values", 
                                         "method")))  %>%
    rename ( geo = nuts_level_2 ) %>%
    filter ( ! geo %in% potentially_imputed_from_nuts_1$geo ) %>%
    bind_rows ( potentially_imputed_from_nuts_1  )
  
  ## Create all possible imputations from NUTS2 >> NUTS3
  potentially_imputed_from_nuts_2 <- full_code_table %>%
    filter ( nuts_level_2 %in% validated_nuts_2$geo ) %>%
    rename ( geo = nuts_level_2 ) %>%
    select ( -all_of(c("nuts_level_1"))) %>%
    left_join ( dat, by = 'geo' ) %>%
    mutate ( method = paste0(
      "imputed from NUTS2 ", 
      geo, " [", method, "]")
    ) %>%
    select ( all_of(c("nuts_level_3", "values", "method"))) %>%
    rename ( geo = nuts_level_3 ) %>%
    filter ( ! geo %in%  validated$geo )
  
  ## Now add possible imputations from country level NUTS0 >> NUTS3
  imputed_from_nuts_2 <- potentially_imputed_from_country %>%
    distinct_at  ( tidyselect::all_of (c("nuts_level_3", "values", 
                                         "method")))  %>%
    rename ( geo = nuts_level_3 ) %>%
    filter ( ! geo %in% potentially_imputed_from_nuts_2$geo ) %>%
    bind_rows ( potentially_imputed_from_nuts_2 )
  
  ## Now add the original data and the the NUTS2 >> NUTS3 imputations
  actual_to_imputed <- validated %>%
    full_join ( imputed_from_nuts_2,
                by = c("geo", "values", "method") ) 
  
  ## Now add whatever can be added from NUTS0 or NUTS1
  imputed_df <- imputed_from_nuts_1 %>%
    filter ( ! geo %in% actual_to_imputed$geo ) %>%
    distinct_at ( tidyselect::all_of(c("geo", "values", "method"))) %>%
    bind_rows (  actual_to_imputed ) %>%
    distinct_at ( tidyselect::all_of( c("geo", "values", "method")) ) 
  
  ## And at last, add NUTS0 >> NUTS1 imputations 
  imputed_df_2 <- potentially_imputed_from_country %>%
    distinct_at ( tidyselect::all_of(c("nuts_level_1", "values", "method"))) %>%
    dplyr::rename ( geo = nuts_level_1 ) %>%
    bind_rows ( imputed_df )
  
  imputed_dfv <- validate_nuts_regions(imputed_df_2,
                                       nuts_year = nuts_year ) %>%
    dplyr::arrange(geo) %>%
    mutate ( ## remove case when method was originally "" and we have [] in the end
      method = gsub("\\s\\[\\]", "", method)
      )
  
  names(imputed_dfv)[which (names(imputed_dfv)=="geo")] <- geo_var
  names(imputed_dfv)[which (names(imputed_dfv)=="values_var")] <- values_var
  
  ## Now only valids are returned.  Maybe all should be.
  imputed_dfv
}
