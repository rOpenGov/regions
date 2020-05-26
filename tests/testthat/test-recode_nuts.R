library(regions)
foo <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" ,
            "HU12", "DED", 
            "FRK"), 
  values = runif(6, 0, 100 ),
  stringsAsFactors = FALSE )

recode_nuts(dat = foo, nuts_year = 2013)

test <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" ,
            "HU12", "DED", 
            "FRK", "FR7"), 
  values = runif(7, 0, 100 ),
  stringsAsFactors = FALSE )

tested1 <- recode_nuts(dat = test, 
                       geo_var = "geo", 
                       nuts_year = 2013)
sort(unique(tested1$geo))

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


test2 <- data.frame ( 
  geo  =  c("FR", "DEE32", "UKI3" ,
            "HU12", "DED", 
            "FRK", "ZZZ"), 
  values = runif(7, 0, 100 ),
  stringsAsFactors = FALSE )

tested2 <- recode_nuts(test2, nuts_year = 2016)

test_that("incorrect codes are identified", {
  expect_equal(tested2[ grepl("Not found", tested2$typology_change), "geo" ], 
               c("ZZZ"))
  expect_equal(tested2[ grepl("Used", tested2$typology_change), "code_2016" ], 
               NA_character_)
})
