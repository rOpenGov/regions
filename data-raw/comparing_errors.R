library(dplyr)
library(tidyr)
library(regions)
library(rsample)


?eurostat::get_eurostat
gdp <- eurostat::get_eurostat('nama_10r_3gdp', time_format = "num" )

gdp_hab <- gdp %>%
  filter ( unit == 'EUR_HAB', 
           time == 2018 ) %>%
  select ( -all_of(c("unit"))) %>%
  validate_nuts_regions()

gdp_hab_3_imputed_from_2 <- gdp_hab %>% 
  filter ( typology == 'nuts_level_2' ) %>%
  mutate ( method = ifelse(!is.na(values), 
                           'actual', 'missing')) %>%
  impute_down_nuts () %>%
  rename ( imputed_nuts_above = values )  %>%
  select ( -all_of(c("valid_2016", "method", "typology")))

gdp_hab_2_imputed_from_1 <- gdp_hab %>% 
  filter ( typology == 'nuts_level_1' ) %>%
  mutate ( method = ifelse(!is.na(values), 
                           'actual', 'missing')) %>%
  impute_down_nuts () %>%
  rename ( imputed_nuts_above = values ) %>%
  select ( -all_of(c("valid_2016", "method", "typology")))


gdp_hab_1_imputed_from_0 <- gdp_hab %>% 
  filter ( typology == 'country' ) %>%
  mutate ( method = ifelse(!is.na(values), 
                           'actual', 'missing')) %>%
  impute_down_nuts () %>%
  rename ( imputed_nuts_above = values )  %>%
  select ( -all_of(c("valid_2016", "method", "typology")))

gdp_hab_3 <- gdp_hab %>% 
  filter ( typology == 'nuts_level_3' ) %>%
  mutate ( method = ifelse(!is.na(values), 
                           'actual', 'missing')) %>%
  left_join ( imputed_from_nuts_2, 
              by = c("geo")) %>%
  mutate ( imputed_median = median(values, na.rm=TRUE)) %>%
  mutate ( error_nuts = imputed_nuts_above - values )


gdp_hab_2 <- gdp_hab %>% 
  filter ( typology == 'nuts_level_2' ) %>%
  mutate ( method = ifelse(!is.na(values), 
                           'actual', 'missing'))  %>%
  left_join ( imputed_from_nuts_1, 
              by = c("geo")) %>%
  mutate ( imputed_median = median(values, na.rm=TRUE)) %>%
  mutate ( error_nuts = imputed_nuts_above - values )

gdp_hab_1 <- gdp_hab %>% 
  filter ( typology == 'nuts_level_1' ) %>%
  mutate ( method = ifelse(!is.na(values), 
                           'actual', 'missing'))  %>%
  left_join ( gdp_hab_1_imputed_from_0 , 
              by = c("geo")) %>%
  mutate ( imputed_median = median(values, na.rm=TRUE)) %>%
  mutate ( error_nuts = imputed_nuts_above - values )


median_value_nuts_3 <- median(gdp_hab_3$values, na.rm=TRUE)
median_value_nuts_2 <- median(gdp_hab_2$values, na.rm=TRUE)


  
s_geo_3 <- sum(gdp_hab_3_imputed$error_geo^2, na.rm=TRUE) / (sum(!is.na(gdp_hab_3_imputed$values))-1)
s_median_3 <- sum(gdp_hab_3_imputed$error_median^2, na.rm=TRUE) / (sum(!is.na(gdp_hab_3_imputed$values))-1)
stdev_geo_3 <- s_geo_3 /  (sum(!is.na(gdp_hab_3_imputed$values)^0.5))
stdev_median_3 <- s_median_3 /  (sum(!is.na(gdp_hab_3_imputed$values)^0.5))
(stdev_geo_3  / stdev_median_3)-1



s_geo_2 <- sum(gdp_hab_2_imputed$error_geo^2, na.rm=TRUE) / (sum(!is.na(gdp_hab_2_imputed$values))-1)
s_median_2 <- sum(gdp_hab_2_imputed$error_median^2, na.rm=TRUE) / (sum(!is.na(gdp_hab_2_imputed$values))-1)
stdev_geo_2 <- s_geo_2 /  (sum(!is.na(gdp_hab_2_imputed$values)^0.5))
stdev_median_2 <- s_median_2 /  (sum(!is.na(gdp_hab_2_imputed$values)^0.5))
(stdev_geo_2  / stdev_median_2)-1
(stdev_geo_3  / stdev_median_3)-1

