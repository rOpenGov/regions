#' Validate Parameter 'dat'
#' @importFrom assertthat assert_that
#' @param dat A data frame input to be validated.
#' @param values_var The variable that contains the upstream data to be
#' imputed to the downstream data, defaults to \code{"values"}.
#' @param geo_var The variable that contains the geographical codes in the
#' NUTS typologies, defaults to code{"geo_var".}
#' @param method_var The variable that contains the metadata on various
#' processing information, defaults to \code{NULL} in which case it will
#' be returned as \code{'method'}.
#' @param nuts_year The year of the NUTS typology to use.
#' @return A logical variable showing if all assertions were met.
#' @keywords internal

validate_data_frame <- function(dat, 
                                geo_var = NULL, 
                                nuts_year = NULL, 
                                values_var = NULL, 
                                method_var = NULL) {
  
  assertthat::assert_that(
    "data.frame" %in% class(dat), 
    msg = "The parameter 'dat' must be a data.frame-like object, like a data frame or a tibble."
  )
  
  assertthat::assert_that (
    nrow(dat) > 0, 
    msg = "The data frame has no observations."
  )
  
  if (!is.null(geo_var)) {
    assertthat::assert_that(
      geo_var %in% names(dat), 
      msg = glue::glue ("geo_var={geo_var} is not among names(dat)")
    )
  }
  
  if (!is.null(values_var)) {
    assertthat::assert_that(
      values_var %in% names(dat), 
      msg = glue::glue ("values_var={values_var} is not among names(dat)")
    )
  }
  
  if (!is.null(method_var)) {
    assertthat::assert_that(
      method_var %in% names(dat), 
      msg = glue::glue ("method_var={method_var} is not among names(dat)")
    )
  }
  
  if (!is.null(nuts_year)) {
    assertthat::assert_that(
      nuts_year %in% c(1999, 2003, 2006, 2010, 2013, 2016, 2021),
      msg = glue::glue ("nuts_year={nuts_year} is an invalid parameter setting.")
    )
  }
}

#' Assertion for Correct Function Calls
#'
#' Assertions are made to give early and precise error messages for wrong
#' API call parameters.
#'
#' These assertions are called from various wrapper functions.  However, you can also call this
#' function directly to make sure that you are adding (programmatically) the correct
#' parameters to a call.
#'
#' All \code{\link{validate_parameters}} parameters default to \code{NULL}.
#' Asserts the correct parameter values for any values that are not \code{NULL}.
#'
#' @param param A parameter value that must not be \code{NULL}.
#' @param param_name The name of the parameter that must not have a value of \code{NULL}.
#' @param typology Currently the following typologies are supported:
#' \code{"NUTS1"}, \code{"NUTS2"}, \code{"NUTS3"} or \code{"NUTS"} for any of
#' the NUTS typologies. The technical typology \code{"NUTS0"}
#' can be used to translate Eurostat country codes to ISO 3166-1 alpha-2
#' country codes.
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @return A boolean, logical variable if the parameter calls are valid.
#' @export

validate_parameters <- function(typology = NULL, 
                                param = NULL, 
                                param_name = NULL) {
  
  ## First make assertions about a mandatory parameter -----------------------
  if (!is.null(param_name)) validate_param ( param = param, 
                                             param_name = param_name )
  
  ## Then make assertions about optional parameters --------------------------
  if (!is.null(typology)) validate_typology(typology)
}

#' Validate Mandatory Parameters
#' 
#' These parameters must not be \code{NULL}. The \code{param_name} is needed for a 
#' meaningful error message.
#'
#' @inheritParams validate_parameters
#' @return A boolean, logical variable if the mandatory parameter is present.
#' @keywords internal
validate_param <- function (param, param_name ) {
  
  assertthat::assert_that(
    ! is.null(param),
      msg = glue::glue("The parameter '{param_name}' must not be NULL.")
  )
  
}

#' Validate typology Parameter
#' 
#' @inheritParams validate_parameters
#' @return A boolean, logical variable if the typology in question exists, the typology parameter is valid.
#' @keywords internal
validate_typology <- function (typology) {
  assertthat::assert_that(
    typology %in% c("NUTS", "NUTS1", "NUTS2", "NUTS3", "NUTS0"),
    msg = "Currently only NUTS-like typologies are supported, i.e., NUTS, NUTS0, NUTS1, NUTS2, NUTS3."
  )
}

