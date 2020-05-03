

test <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" ,
            "HU12", "DED", 
            "FRK", "FR7"), 
  values = runif(7, 0, 100 ),
  stringsAsFactors = FALSE )

dat <- test 
tested1 <- recode_nuts(test)

test_that("all geo codes are returned", {
  expect_equal(sort(unique(tested1$geo)), sort(unique(test$geo)))
})

test_that("correct values are returned", {
  expect_equal(tested1[ grepl("Used", tested1$typology_change), "geo" ], 
               c("HU12", "DEE32"))
  expect_equal(tested1[ grepl("Used", tested1$typology_change), "code_2013" ], 
               c(NA_character_, NA_character_))
  expect_equal(tested1[ "FRK" == tested1$geo, "code_2013" ], 
               "FR7")
})
