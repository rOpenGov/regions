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
#' @return A boolean if the parameter matches the Spotify Web API parameter range.
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
#' @return A boolean if the parameter matches the Spotify Web API parameter range.
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
#' @return A boolean if the parameter matches the Spotify Web API parameter range.
#' @keywords internal
validate_typology <- function (typology) {
  assertthat::assert_that(
    typology %in% c("NUTS", "NUTS1", "NUTS2", "NUTS3", "NUTS0"),
    msg = "Currently only NUTS-like typologies are supported, i.e., NUTS, NUTS0, NUTS1, NUTS2, NUTS3."
  )
}

