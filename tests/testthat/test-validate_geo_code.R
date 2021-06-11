

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

test_that("invalid dates give error message", {
  expect_error(validate_geo_code (geo = c("BE1", "HU102"),
                                  nuts_year = 2002))
  expect_error (validate_geo_code (geo = 210,
                                   nuts_year = 2013))
})

test_that("Proper answers returned", {
  expect_true (all(validate_geo_code (c(
    "DE1", "DE7", "HU1", "BE1"
  ), 2016) == rep("nuts_level_1", 4)))
  expect_equal (validate_geo_code (c("EL", "XK", "GB")),
                c("country", "non_eu_country", "iso_country"))
})
