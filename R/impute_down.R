#' Imputing Data From Larger To Smaller Units
#'
#' @param upstream_data An upstream data frame to project on containing
#' smaller geographical units, for example, country-level data.
#' @param downstream_data A downstream data frame containing the
#' smaller level missing data observations. It must contain all the
#' necessary structural information for imputation.
#' @param country_var The geographical ID of the upstream data,
#' defaults to \code{"country_code"}.
#' @param regional_code The geographical ID of the downstream data,
#' defaults to \code{"geo_code"}.
#' @param values_var The variable that contains the upstream data to be
#' imputed to the downstream data, defaults to \code{"values"}.
#' @param time_var The time component, if present, defaults to
#' \code{"year"}.
#' @param upstream_method_var The name of the variable that contains the
#' potentially applied imputation methods. Defaults to \code{NULL}.
#' @param downstream_method_var The name of the variable that will contain
#' the metadata of the potentially applied imputation methods.
#' Defaults to \code{NULL} in which case a variable called \code{'method'}
#' will be created.  If possible, avoid using \code{upstream_data} or
#'  \code{downstream_data} that contains a variable called \code{'method'}
#' for other purposes.
#' @importFrom dplyr left_join rename mutate_if mutate
#' @family impute functions
#' @return The upstream data frame (containing data of a larger unit) and
#' the downstream data (containing data of smaller sub-divisional units) are
#' joined; whenever data is missing in the downstream sub-divisional column,
#' it is imputed with the corresponding values from the upstream data frame.
#' The \code{'method'} metadata column explains if the actual downstream
#' data or the imputed data can be found in the downstream value column.
#' @examples{
#' upstream <- data.frame ( country_code =  rep( "AU", 3),
#'                          year = c(2018:2020),
#'                          my_var  = c(10,12,11),
#'                          description = c("note1", NA_character_,
#'                          "note3")
#'                        )
#'
#' downstream <- australia_states
#'
#' impute_down ( upstream_data  = upstream,
#'               downstream_data = downstream,
#'               country_var = "country_code",
#'               regional_code = "geo_code",
#'               values_var = "my_var",
#'               time_var = "year" )
#' }
#' @export

impute_down <- function(upstream_data = NULL,
                        downstream_data = NULL,
                        country_var = "country_code",
                        regional_code = "geo_code",
                        values_var = "values",
                        time_var = NULL,
                        upstream_method_var = NULL,
                        downstream_method_var = NULL) {
  ## non-standard evaulation vars -----------------------------
  impute_values <- country_code <- NULL
  . <- impute_method <- method <- values <- NULL
  
  ## upstream data --------------------------------------------
  upstream_df <-
    dplyr::mutate_if(upstream_data, is.factor, as.character)
  if (country_var %in% names(upstream_df)) {
    names(upstream_df[which(names(upstream_df) == country_var)]) <-
      'country_code'
  } else {
    stop ("The 'country_var' is not found in the upstream data.")
  }
  
  if (values_var %in% names(upstream_df)) {
    names(upstream_df)[which(names(upstream_df) == values_var)] <-
      'impute_values'
  } else {
    stop ("The 'values_var' is not found in the upstream data.")
  }
  
  if (!is.null(time_var)) {
    if (time_var %in% names(upstream_df)) {
      names(upstream_df)[which(names(upstream_df) == time_var)] <- 'time'
      no_time_var <- FALSE
    } else {
      stop("The 'time_var' is not present in the upstream data.")
    }
  } else  {
    no_time_var <- TRUE
  }
  
  if (!is.null(upstream_method_var)) {
    ## upstream method name is given
    if (upstream_method_var %in% names(upstream_df)) {
      names(upstream_df)[which(names(upstream_df) == upstream_method_var)] <-
        'impute_method'
    } else {
      stop("The 'upstream_method_var' is not present in the upstream data.")
    }
  } else  {
    possible_method_vars <- ifelse (
      is.null(downstream_method_var),
      yes = "method",
      no = c("method", downstream_method_var)
    )
    
    if (any (names(upstream_df) %in% possible_method_vars)) {
      # case for potential naming conflict ------------
      conflicting_var <-
        which(names(upstream_df) %in% possible_method_vars)
      conflicting_variable_name <-
        names(upstream_df)[conflicting_var]
      new_variable_name <-
        
        paste0(conflicting_variable_name, "_upstream")
      warning(
        "The upstream_df has a variable called '",
        conflicting_variable_name ,
        "' that will be changed to\n'",
        new_variable_name,
        "' to avoid confusion.",
        "This is likely to be a logical error, check your upstream and downstream data frames."
      )
      names (upstream_df)[conflicting_var] <- new_variable_name
    }
    upstream_df$impute_method <- paste0("actual")
  }
  
  ### Downstream data ---------------------------------------------------
  downstream_df <-
    dplyr::mutate_if(downstream_data, is.factor, as.character)
  if (country_var %in% names(downstream_df)) {
    names(downstream_df)[which(names(downstream_df) == country_var)] <-
      'country_code'
  } else {
    stop ("The 'country_var' is not found in the downstream data.")
  }
  
  if (regional_code %in% names(downstream_df)) {
    names(downstream_df)[which(names(downstream_df) == regional_code)] <-
      'geo_code'
  } else {
    stop ("The 'regional_code' is not found in the downstream data.")
  }
  
  if (country_var %in% names(downstream_df)) {
    names(downstream_df)[which(names(downstream_df) == country_var)] <-
      'country_code'
  } else {
    stop ("The 'country_code' is not found in the downstream data.")
  }
  
  if (!values_var %in% names(downstream_df)) {
    downstream_df$values <- NA_real_
  }
  
  if (!is.null(downstream_method_var)) {
    if (downstream_method_var %in% names(downstream_df)) {
      names(downstream_method_df)[which(names(downstream_df) == downstream_method_var)] <-
        'method'
    } else {
      stop("The 'downstream_method_var' is not present in the downstream data.")
    }
  } else  {
    downstream_df$method <- ifelse (is.na(downstream_df$values),
                                    yes  = NA_character_,
                                    no = "actual")
  }
  
  ## Checking if all temporary data frames are correctly created -----------
  if (!all(c("country_code", "impute_values", "impute_method") %in% names (upstream_df))) {
    stop("The 'upstream_df' is not well formatted. Please raise an issue on github.")
  }
  
  if (!all(c("country_code", "geo_code", "values", "method") %in% names (downstream_df))) {
    stop("The 'downstream_df' is not well formatted. Please raise an issue on github.")
  }
  
  
  ## Creating an empty grid --------------------------------------
  if (no_time_var) {
    return_df <- expand.grid(unique (downstream_df$geo_code),
                             stringsAsFactors = FALSE)
    names(return_df) <- "geo_code"
  } else {
    return_df <- expand.grid(unique (downstream_df$geo_code),
                             unique (upstream_df$time),
                             stringsAsFactors = FALSE)
    
    names(return_df) <- c("geo_code", "time")
  }
  
  if (no_time_var) {
    join_by_vars <- c("country_code")
  } else {
    join_by_vars <- c("country_code", "time")
  }
  
  ## Joining the data and filling in the missing data ----------------
  return_df <- dplyr::left_join (return_df, downstream_df,
                                 by = 'geo_code') %>%
    dplyr::left_join (upstream_df, by = join_by_vars) %>%
    dplyr::mutate (method = ifelse (
      is.na(values) & !is.na(impute_values),
      yes = paste0("imputed from ", country_code, " ",
                   impute_method),
      no = method
    )) %>%
    dplyr::mutate (values = ifelse (
      is.na(values) & !is.na(impute_values),
      yes = impute_values,
      no =  values
    )) %>%
    dplyr::mutate (method = ifelse (is.na(values),
                                    yes = "missing",
                                    no = method))
  
  ## Adding the original variable names back  ----------------------
  
  names(return_df)[which(names(return_df) == "time")] <- time_var
  names(return_df)[which(names(return_df) == "values")] <-
    values_var
  names(return_df)[which(names(return_df) == "country_code")] <-
    country_var
  names(return_df)[which(names(return_df) == "geo_code")] <-
    regional_code
  return_df[which(names(return_df) %in% c("impute_values", "impute_method"))] <-
    NULL
  return_df
}
