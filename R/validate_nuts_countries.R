#' @title Validate Conformity with NUTS Country Codes
#'
#' @description This function is mainly a wrapper around the well-known
#'  \link[countrycode]{countrycode} function, with three exception that are
#'  particular to the European Union statistical nomenclature.
#' @details All ISO-3166-1 country codes are validated, and also the
#' three exceptions.
#' 
#' \describe{
#'   \item{EL}{Treated valid, because NUTS uses EL instead of GR for Greece since 2010.}
#'   \item{UK}{Treated valid, because NUTS uses UK instead of GB for the United Kingdom.}
#'   \item{XK}{XK is used for Kosovo, because Eurostat uses this code, too.}
#' }
#' @param dat A data frame with a 2-character geo variable to be validated
#' @param geo_var Defaults to \code{"geo"}. The variable that contains the
#' 2 character geo codes to be validated.
#' @importFrom dplyr mutate rename select mutate_if case_when left_join
#' @importFrom tidyselect all_of
#' @importFrom countrycode countrycode
#' @importFrom purrr quietly
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @return The original data frame extended with the column \code{'typology'}.
#' This column states \code{'country'} for valid country typology coding, or
#' appropriate label for invalid ISO-3166-alpha-2 and ISO-3166-alpha-3 codes.
#' @family validate functions
#' @examples{
#' my_dat <- data.frame (
#'  geo = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ),
#'  values = runif(8)
#'  )
#'
#'  ## NLD is an ISO 3-character code and is not validated.
#'  validate_nuts_countries(my_dat)
#' }
#' @export

validate_nuts_countries <- function (dat, geo_var = "geo") {
  
  validate_data_frame(dat, geo_var = geo_var )

  dat <-  dat %>%
    mutate_if (is.factor, as.character)

  if (!"typology" %in% names(dat)) {
    dat$typology <- NA_character_
  }
  
  original_names <- names(dat)
  
  quiet_country_codes <- function(x) {
    res <- purrr::quietly(countrycode::countrycode)(x, "iso2c", "iso3c")
    res$result
  }
  
  iso_2c <- as.character(dat[, geo_var])
  
  validate_country_df <- dat %>%
    select ( all_of(original_names) ) %>%
    mutate (iso2c = iso_2c) %>%
    mutate (
      iso2c = case_when (
        .data$iso2c == "UK" ~ "GB",
        .data$iso2c == "EL" ~ "GR",
        .data$iso2c == "XK" ~ "GR",
        #only to avoid warning
        TRUE ~ .data$iso2c
      )
    ) %>%
    mutate (iso3c = quiet_country_codes (.data$iso2c)) %>%
    mutate (validation_n_char = nchar(.data$iso2c)) %>%
    mutate (
      typology = case_when (
        is.na (.data$iso3c) &
          validation_n_char == 2 ~ "invalid_iso-3166-alpha-2",
        is.na (.data$iso3c) &
          validation_n_char == 3 ~ "iso-3166-alpha-3",!is.na(.data$iso3c) &
          validation_n_char == 2 ~ "country",
        is.na (.data$iso3c) & validation_n_char != 2  ~ typology
      )
    )
  
  validate_country_df %>%
    dplyr::select (all_of(original_names))
}
