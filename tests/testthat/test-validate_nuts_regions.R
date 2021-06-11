my_reg_data <- data.frame (
  geo = c(
    "BE1",
    "HU102",
    "FR1",
    "FRB",
    "DED",
    "FR7",
    "TR",
    "DED2",
    "EL",
    "XK",
    "GB"
  ),
  values = runif(11)
)

validate_nuts_regions (dat = my_reg_data)

result1 <- validate_nuts_regions (dat = data.frame (geo = c("BE1", "HU102")),
                                  nuts_year = 2003)

test_that("correct typology is returned", {
  expect_equal(result1$typology, c("nuts_level_1", NA_character_))
})

test_that("invalid dates give error message", {
  expect_error(validate_nuts_regions (dat = data.frame (geo = c("BE1", "HU102")), nuts_year = 2002))
  expect_error (validate_nuts_regions (dat = "c",
                                       nut_year = 2013))
  expect_error (validate_nuts_regions (dat = data.frame (
    geo = c("HU102", "CZ1"),
    values = c(1, 2)
  ), geo_var = "country"))
})

with_nuts_df <- data.frame (
  nuts = c("DE1", "DE7", "HU1", "BE1"),
  values = c(1:4),
  typology = rep("nuts1", 4)
)

tested_with_nuts <- validate_nuts_regions(dat = with_nuts_df,
                                          geo_var = "nuts")

test_that("Special column names do not cause confusion", {
  expect_equal(all(tested_with_nuts$valid_2016), TRUE)
  expect_equal(ncol(tested_with_nuts), 5)
})
