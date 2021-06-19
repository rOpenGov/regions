#' NUTS Coding Exceptions
#'
#' A dataset containing exceptions to the NUTS geographical codes.  
#' 
#' They contains non-EU 
#' regions that are consistent with NUTS, but not defined within the NUTS.
#' 
#' The also contain European country codes that do not conform with NUTS.
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{geo}{National and sub-national geographical codes from Eurostat}
#'   \item{typology}{Short description of exception}
#' }
#' @source Eurostat NUTS history: \url{https://ec.europa.eu/eurostat/web/nuts/history/}
#' @seealso nuts_recoded, nuts_changes, all_valid_nuts_codes
"nuts_exceptions"
