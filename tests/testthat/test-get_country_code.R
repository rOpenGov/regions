

test_that("Correct NUTS codes are handled", {
  expect_equal(get_country_code (c("EL", "GR", "DED", "HU102")), 
               c("GR", "GR", "DE", "HU"))
})


test_that("errors work", {
  expect_error(get_country_code (c("EL", "GR", "DED", "HU102"), 
                                 typology = "OECD"))
})
