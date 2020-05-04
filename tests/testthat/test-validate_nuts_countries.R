
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

dat = small_df[1:6,]

test_that("Correct structure returned", {
  expect_equal(names(validate_nuts_countries(dat = small_df[1:6,])), 
               c("geo", "values", "notes", "typology"))
})

test_that("Error handling workds", {
  expect_error( validate_nuts_countries(dat = "character"))
  expect_error( validate_nuts_countries(dat = small_df [-(1:8), ]))
})


test_df_2 <- data.frame ( 
  values = runif(8), 
  countries = c("AL", "GR", "XK", "EL", "UK", "GB", "NLD", "ZZ" ), 
  time  = rep(2018,8), 
  notes = paste0("notes", 1:8))


tested2 <- validate_nuts_countries(test_df_2, geo_var = "countries")

test_that("Different column order works", {
  expect_equal(tested2$typology,
               c(rep("country",6), "iso-3166-alpha-3", 
                 "invalid_iso-3166-alpha-2"))
})
