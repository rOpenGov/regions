#' Google Mobility Report European Correspondence Table 
#'
#' A dataset containing the correspondence table between the EU 
#' NUTS 2016 typology and the typology used by Google in the
#' Google Mobility Reports.
#' 
#' In some cases only a full correspondence is not possible. In these
#' cases we created pseudo-NUTS codes, which have a \code{FALSE}
#' \code{valid_2016} value. These pseudo-NUTS codes can help 
#' approximation for the underlying regions.
#' 
#' Pseudo-NUTS codes were used in Estonia, Italy, Portugal, Slovenia
#' and in parts of Latvia, which is not finalized.
#' 
#' @format A data frame with 813 rows and 6 variables:
#' \describe{
#'   \item{country_code}{ISO 3166-1 alpha2 code}
#'   \item{google_region_level}{Hierarchical level in the Google Mobility Reports}
#'   \item{google_region_name}{The name used by Google.}
#'   \item{code_2016}{NUTS code in the 2016 definition}
#'   \item{typology}{country, NUTS1, NUTS2 or NUTS3}
#'   \item{valid_2016}{Logical variable, if the coding is valid in 
#'   NUTS2016}
#' }
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
#' @author Istvan Zsoldos, Daniel Antal
"google_nuts_matchtable"
