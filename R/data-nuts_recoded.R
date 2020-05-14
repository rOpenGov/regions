#' European Union: Recoded NUTS units 1995-2021. 
#'
#' Containing all recoded NUTS units from the European Union. 
#' This is re-arranged from \code{\link{nuts_changes}}.
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{geo}{NUTS geo identifier}
#'   \item{typology}{country, NUTS1, NUTS2 or NUTS3}
#'   \item{nuts_year}{year of the NUTS definition or version}
#'   \item{change_year}{when the geo code changed}
#'   \item{iso2c}{Two character ISO standard country codes.}
#' }
#' @seealso nuts_changes, all_valid_nuts_codes
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
"nuts_recoded"
