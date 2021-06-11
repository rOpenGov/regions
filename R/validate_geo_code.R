#' Validate Conformity with NUTS Geo Codes (vector)
#'
#' Validate that \code{geo} is conforming with the \code{NUTS1},
#' \code{NUTS2}, or \code{NUTS3} typologies.
#' While country codes are technically not part of the NUTS typologies,
#' Eurostat de facto uses a \code{NUTS0} typology to identify countries.
#' This de facto typology has three exception which are handled by the
#' \link[regions]{validate_nuts_countries} function.
#'
#' NUTS typologies have different versions, therefore the conformity
#' is validated with one specific versions, which can be any of these:
#' \code{1999}, \code{2003}, \code{2006}, \code{2010},
#'  \code{2013}, the currently used \code{2016} and the already
#'  announced and defined \code{2021}.
#'
#' The NUTS typology was codified with the \code{NUTS2003}, and the
#' pre-1999 NUTS typologies may confuse programmatic data processing,
#' given that some  NUTS1 regions were identified with country codes
#' in smaller countries that had no \code{NUTS1} divisions.
#'
#' Currently the \code{2016} is used by Eurostat, but many datasets
#' still contain \code{2013} and sometimes earlier metadata.
#' @param geo A vector of geographical code to validate.
#' @param nuts_year A valid NUTS edition year.
#' @importFrom tidyselect all_of
#' @importFrom purrr set_names
#' @importFrom utils data
#' @importFrom glue glue
#' @importFrom dplyr distinct_at mutate filter if_else select bind_rows
#' @importFrom dplyr full_join
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @return A character list with the valid typology, or 'invalid' in the cases
#' when the geo coding is not valid.
#' @examples{
#' my_reg_data <- data.frame (
#'   geo = c("BE1", "HU102", "FR1",
#'           "DED", "FR7", "TR", "DED2",
#'           "EL", "XK", "GB"),
#'   values = runif(10))
#'
#' validate_geo_code(my_reg_data$geo)
#' }
#' @export

validate_geo_code <- function (geo, nuts_year = 2016) {
  all_valid_nuts_codes <- NULL
  
  assertthat::assert_that(any (c("character", "factor") %in% class(geo)),
                          msg = "geo must be a character or factor vector.")
  
  geo <- as.character(geo)
  
  assertthat::assert_that(
    nuts_year %in% c(1999, 2003, 2006, 2010, 2013, 2016, 2021),
    msg = glue::glue ("nuts_year={nuts_year} is an invalid parameter setting.")
  )
  
  utils::data (all_valid_nuts_codes,
               package = "regions",
               envir = environment())
  
  exceptions <- all_valid_nuts_codes %>%
    mutate (country_code = get_country_code(geo          = .data$geo,
                                            typology     = "NUTS")) %>%
    filter (.data$country_code %in% c("IS", "LI", "NO", "AL",
                                      "CH", "MK", "RS", "TR",
                                      "ME"))  %>%
    distinct (.data$geo, .data$typology) %>%
    mutate (typology = glue::glue ("non_eu_{typology}")) %>%
    select (all_of(c("geo", "typology"))) %>%
    bind_rows (tibble (
      geo = c("GB", "GR", "XK"),
      typology = c(rep("iso_country", 2), "non_eu_country")
    ))
  
  filtering <- grepl(as.character(nuts_year),
                     all_valid_nuts_codes$nuts)
  
  filtered_nuts_data_frame <- all_valid_nuts_codes[filtering,] %>%
    select (all_of(c("geo", "typology"))) %>%
    full_join (exceptions,
               by = c("geo", "typology"))
  
  tibble::tibble (geo = geo) %>%
    left_join (filtered_nuts_data_frame, by = 'geo') %>%
    mutate (typology = if_else (
      condition = is.na(.data$typology),
      true = "invalid",
      false = as.character(.data$typology)
    )) %>%
    select (all_of("typology")) %>%
    unlist () %>%
    as.character()
  
}
