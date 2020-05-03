
test_df <- data.frame ( 
  geo = c('AL', 'AT', 'BE', 'BG', 'CH', 'CY',
          'CZ', 'DE', 'DK', 'EE', 'EL', 'ES', 
          'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 
          'IS', 'IT', 'LI', 'LT', 'LU', 'LV', 
          'ME', 'MK', 'MT', 'NL', 'NO', 'PL', 
          'PT', 'RO', 'RS', 'SE', 'SI', 'SK',
          'TR', 'UK', "XK", "NLD", "ZZ" ))

small_df <- data.frame ( 
  geo = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ), 
  values = runif(8), 
  notes = paste0("notes", 1:8))

test_that("multiplication works", {
  expect_equal(validate_nuts_country(small_df)$typology, 
               c(rep ("country",6), NA_character_, "invalid_country_code")
  )
})

test_that("nuts_country_validation_works", {
  expect_equal(validate_nuts_country (small_df)$typology, 
               c(rep ("country",6), NA_character_, "invalid_country_code")
  )
})


test_that("Correct structure returned", {
  expect_equal(names(validate_nuts_country(dat = small_df[1:6,])), 
               c("geo", "values", "notes", "typology"))
})

test_that("Error handling workds", {
  expect_error( validate_nuts_country(dat = "character"))
  expect_error( validate_nuts_country(dat = small_df [-(1:8), ]))
})


