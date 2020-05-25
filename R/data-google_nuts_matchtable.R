#' Google Mobility Report European Correspondence Table 
#'
#' A dataset containing the correspondence table between the EU 
#' NUTS 2016 typology and the typology used by Google in the
#' Google Mobility Reports.
#'
#' @format A data frame with 724 rows and 5 variables:
#' \describe{
#'   \item{country_code}{ISO 3166-1 alpha2 code}
#'   \item{google_region_level}{Hierarchical level in the Google Mobility Reports}
#'   \item{google_region_name}{The name used by Google.}
#'   \item{code_2016}{NUTS code in the 2016 definition}
#'   \item{typology}{country, NUTS1, NUTS2 or NUTS3}
#' }
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
#' @author Istvan Zsoldos
"google_nuts_matchtable"
