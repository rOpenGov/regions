library(dplyr)
library(tidyr)
library(regions)
coal_directory <- "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2020 Projektek/coal/data"

eb15 <- readRDS( file.path(coal_directory, "eb15.rds"))
dat <- dplyr::sample_n (eb15, 20)
names ( dat )
geo_var <- "region_nuts_codes"
nuts_year = 2016

validate_nuts_countries(dat, geo_var = 'region_nuts_codes')
validated <- validate_nuts_regions(dat, geo_var = 'region_nuts_codes')
recoded <- recode_nuts(dat, geo_var = 'region_nuts_codes')
