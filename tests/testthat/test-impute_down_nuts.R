
data(mixed_nuts_example)

tested_imputation1 <- impute_down_nuts(mixed_nuts_example)

luxembourg <- tested_imputation1[ substr (tested_imputation1$geo,1,2)=="LU",]

test_that("all units present", {
  expect_equal(luxembourg$geo, c("LU", "LU0", "LU00", "LU000", 
                                 "LUZ", "LUZZ", "LUZZZ"))
})

test_that("correct values are imputed", {
  expect_equal(all ( luxembourg$values == as.numeric(
    mixed_nuts_example[ mixed_nuts_example$geo == "LU", "values"]
  ) ))
})