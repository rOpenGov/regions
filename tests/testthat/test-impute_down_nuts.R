library(testthat)
library(regions)
data(mixed_nuts_example)

dat = mixed_nuts_example
geo_var = "geo"
values_var = "values"
method_var = "method"
nuts_year = 2016

tested_imputation1 <- impute_down_nuts( dat = mixed_nuts_example, 
                                        geo_var = "geo", 
                                        values_var = "values",
                                        method_var = "method",
                                        nuts_year = 2016)

luxembourg <- tested_imputation1[ substr (tested_imputation1$geo,1,2)=="LU",]

tested_imputation_mt <- impute_down_nuts( 
  dat = data.frame ( geo = "MT", 
                     values = 12)
)

test_that("all units present", {
  expect_equal(luxembourg$geo, c("LU", "LU0", "LU00", "LU000", 
                                 "LUZ", "LUZZ", "LUZZZ"))
  expect_equal(tested_imputation_mt$geo, c("MT", "MT0", "MT00", "MT001",
                                          "MT002", "MTZ", "MTZZ", "MTZZZ"))
})

luxembourg_values <- mixed_nuts_example[ mixed_nuts_example$geo == "LU", "values"]

test_that("correct values are imputed", {
  expect_equal(all (luxembourg$values == as.numeric(luxembourg_values)),
               TRUE)
  expect_equal(all (tested_imputation_mt$values == 12), TRUE)
})

test_that("method var is correctly filled", {
  expect_equal(tested_imputation_mt$method, 
               c("", rep("imputed from country MT", 7)))
})

