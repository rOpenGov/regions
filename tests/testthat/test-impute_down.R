upstream1 <- data.frame (
  country_code =  rep("AU", 3),
  year = c(2018:2020),
  my_var  = c(10, 12, 11),
  description = c("note1", NA_character_,
                  "note3")
)

upstream2 <- data.frame (
  country_code =  c(rep("AU", 2),
                    rep("DE", 2)),
  year = rep(c(2018:2019), 2),
  values  = c(10, 12, 8, 10),
  method = rep("actual", 4),
  description = c("note1", NA_character_,
                  "note3", NA_character_)
)

utils::data('australia_states', package = 'regions')
downstream1 <- australia_states


downstream2 <- data.frame (
  country_code = c("AU", "AU", "DE", "DE"),
  geo_code = c("AU-NSW", "AU-QLD", "DE1", "DE2"),
  geo_name = c("New South Wales", "Queensland",
               "Baden-Württemberg",
               "Bayern")
)

downstream3 <- data.frame (
  country_code = c("AU", "AU", "DE", "DE"),
  geo_code = c("AU-NSW", "AU-QLD", "DE1", "DE2"),
  geo_name = c("New South Wales", "Queensland",
               "Baden-Württemberg",
               "Bayern"),
  values = c(NA_real_, 9, 9, NA_real_)
)


upstream_data <- NULL
downstream_data <- NULL
country_var <- "country_code"
regional_code <- "geo_code"
values_var <- "values"
time_var <- NULL
upstream_method_var <- NULL
downstream_method_var <- NULL

test_that("exception handling (erros) work", {
  expect_error(
    impute_down (
      upstream_data  = upstream1,
      downstream_data = downstream1,
      values_var = 'values',
      country_var = 'country_code',
      time_var = 'year'
    )
  )
  expect_error(
    impute_down (
      upstream_data  = upstream1,
      downstream_data = downstream1,
      regional_code = "regcode",
      values_var = 'my_var',
      country_var = 'country_code',
      time_var = 'year'
    )
  )
  expect_error(
    impute_down (
      upstream_data  = upstream1,
      downstream_data = downstream1,
      regional_code = "regcode",
      values_var = 'my_var',
      country_var = 'country_code',
      time_var = 'time'
    )
  )
  expect_error(
    impute_down (
      upstream_data  = upstream1,
      downstream_data = downstream1,
      regional_code = "regcode",
      values_var = 'my_var',
      country_var = 'country',
      time_var = 'year'
    )
  )
  expect_error(
    impute_down (
      upstream_data  = upstream1,
      downstream_data = downstream1,
      regional_code = "regcode",
      values_var = 'my_var',
      country_var = 'country_code',
      time_var = 'year',
      upstream_method_var = "upmethod"
    )
  )
  expect_error(
    impute_down (
      upstream_data  = upstream1,
      downstream_data = downstream1,
      regional_code = "regcode",
      values_var = 'my_var',
      country_var = 'country_code',
      time_var = 'year',
      downstream_method_var = "downmethod"
    )
  )
})


imputed1  <- impute_down (
  upstream_data  = upstream1,
  downstream_data = downstream1,
  values_var = 'my_var',
  country_var = 'country_code',
  time_var = 'year'
)

impute2 <- impute_down (
  upstream_data  = upstream2,
  downstream_data = downstream2,
  time_var = NULL,
  country_var = "country_code",
  regional_code = "geo_code",
  values_var = "values",
  upstream_method_var = "method"
)

test_that("warning works", {
  expect_warning(
    impute_down (
      upstream_data  = upstream2,
      downstream_data = downstream2,
      time_var = NULL,
      country_var = "country_code",
      regional_code = "geo_code",
      values_var = "values"
    )
  )
  
})

impute3 <- impute_down (
  upstream_data  = upstream2,
  downstream_data = downstream3,
  time_var = 'year',
  country_var = "country_code",
  regional_code = "geo_code",
  values_var = "values",
  upstream_method_var = 'method'
)


upstream4 <- data.frame (
  country_code =  c(rep("AU", 2),
                    rep("DE", 2)),
  year = rep(c(2018:2019), 2),
  values  = c(NA_real_, 12, 8, 9),
  method = rep("actual", 4),
  description = c("note1", NA_character_,
                  "note3", NA_character_)
)

impute4 <- impute_down (
  upstream_data  = upstream4,
  downstream_data = downstream3,
  time_var = 'year',
  country_var = "country_code",
  regional_code = "geo_code",
  values_var = "values",
  upstream_method_var = 'method'
)

test_that("impute_down gives correct values", {
  expect_equal(imputed1$my_var, c(rep(10, 8), rep(12, 8),
                                  rep(11, 8)))
  expect_equal(impute2[impute2$country_code == "DE" &
                         impute2$year == 2019, "values"], c(10, 10))
  expect_equal(impute3[impute3$country_code == "DE" &
                         impute3$year == 2019, "values"], c(9, 10))
})

test_that("impute_down gives correct methods", {
  expect_equal(
    impute4$method,
    c(
      'missing',
      'actual',
      'actual',
      'imputed from DE actual',
      'imputed from AU actual',
      'actual',
      'actual',
      'imputed from DE actual'
    )
  )
  expect_equal(all(
    c(
      "geo_code",
      "country_code",
      "values",
      "method",
      "description",
      "geo_name",
      "year"
    ) %in% names(impute2)
  ),
  TRUE)
  expect_equal(any(
    c("method.y", "method.x", "values.x", "year.x") %in% names(impute2)
  ),
  FALSE)
  expect_equal(all(grepl("imputed from", impute2$method)),
               TRUE)
})

data("mixed_nuts_example")

test_that("vignette example works", {
  expect_equal(nrow(impute_down_nuts (dat = mixed_nuts_example)), 150)
})