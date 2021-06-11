#' Example Data Frame: Mixed EU Typologies.
#'
#' This data frame is a fictious example that contains in a small,
#' easy-to-review example many potential typological problems. It is
#' used to test imputation functions and to create examples with them.
#'
#' @format A data frame with 22 rows and 3 variables:
#' \describe{
#'   \item{geo}{NUTS geo identifier, mixed from 4 typology levels.}
#'   \item{values}{Random numbers.}
#'   \item{method}{Descriptive metadata.}
#' }
#' @seealso nuts_changes, all_valid_nuts_codes, impute_down_nuts
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
"mixed_nuts_example"
