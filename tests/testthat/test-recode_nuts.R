

test <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" , "HU12", "DED", "FRK", "FR7"), 
  values = runif(7, 0, 100 ),
  stringsAsFactors = F)

recode_nuts(test)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})