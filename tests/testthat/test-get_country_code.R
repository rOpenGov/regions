

test_string <- c("EL", "GR", "DED", "HU102")

test_that("Correct NUTS codes are handled", {
  expect_equal(get_country_code (test_string),
               c("GR", "GR", "DE", "HU"))
})


test_that("errors work", {
  expect_error(get_country_code (test_string,
                                 typology = "OECD"))
})
