

broadband_hh <- eurostat::get_eurostat (id = "isoc_r_broad_h", 
                                        time_format = "num")

gdp <- eurostat::get_eurostat("nama_10r_2gdp",
                              time_format = "num")

library(regions)
isoc_r_iuse_i<- eurostat::get_eurostat("isoc_r_iuse_i",
                       time_format = "num") 

names ( daily_internet_ )
daily_internet_users <- isoc_r_iuse_i  %>%
  dplyr::filter ( unit == "PC_IND", 
                  indic_is == "I_IDAY") %>%
  dplyr::select (geo, time, values )

usethis::use_data ( daily_internet_users, overwrite = TRUE)


paste (sort (unique (broadband_hh$time)), collapse =(", "))

dat <- test

test <- daily_internet  %>% 
  dplyr::mutate ( country_code = get_country_code(geo = geo) ) %>%
  dplyr::filter ( time %in% c(2012, 2018),
                  country_code %in% c("FR", "HU", "LT")) %>%
  dplyr::mutate ( time = paste0("Y", time )) %>%
  tidyr::pivot_wider (., names_from ="time", values_from = "values") %>%
  validate_nuts_region() %>%
  validate_nuts_region(.,  nuts_year = 2010 )

test [ ! test$valid_2010, ]
