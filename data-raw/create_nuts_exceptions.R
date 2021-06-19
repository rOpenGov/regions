library(regions)

create_exceptions <- function () {
  
  utils::data (all_valid_nuts_codes,
               package = "regions",
               envir = environment())
  
  exceptions <- all_valid_nuts_codes %>%
    mutate (country_code = get_country_code(
      geo          = .data$geo,
      typology     = "NUTS") ) %>%
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
  
  exceptions
}

nuts_exceptions <- create_exceptions()

usethis::use_data ( nuts_exceptions , overwrite = TRUE ) 
