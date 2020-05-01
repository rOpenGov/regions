

my_reg_data <- data.frame ( 
  geo = c("BE1", "HU102", "FR1", "FRB",
          "DED", "FR7", "TR", "DED2",
          "EL", "XK", "GB"), 
  values = runif(11) )

validate_nuts_region ( dat = my_reg_data )

result1 <- validate_nuts_region ( 
  dat = data.frame ( 
  geo =c("BE1", "HU102")), 
  nuts_year = 2003 )

test_that("correct typology is returned", {
  expect_equal(
  result1$typology, c("nuts_level_1", NA_character_)
  )
  })

test_that("invalid dates give error message", {
  expect_error(validate_nuts_region ( dat = data.frame ( 
    geo =c("BE1", "HU102")), nuts_year = 2002 )
  )
})
