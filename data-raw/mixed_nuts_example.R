library(regions)
library(tidyverse)

data("all_valid_nuts_codes")

View ( all_valid_nuts_codes)

random_sample <- dplyr::sample_n(all_valid_nuts_codes %>%
                                   filter ( nuts == "code_2016", 
                                            typology == "nuts_level_2"), 10 ) %>%
  rbind (dplyr::sample_n(all_valid_nuts_codes %>%
                           filter ( nuts == "code_2016", 
                                    typology == "nuts_level_1"), 6 )  )  %>%
  rbind (dplyr::sample_n(all_valid_nuts_codes %>%
                           filter ( nuts == "code_2016", 
                                    typology == "nuts_level_3"), 3 )  ) %>%
  rbind (all_valid_nuts_codes %>%
           filter ( geo %in% c("LU", "MT", "HU"), 
                    nuts == "code_2016"))

mixed_nuts_example <- random_sample %>%
  select ( geo ) %>%
  mutate ( values = runif ( nrow(.), 1,100))

mixed_nuts_example <- mixed_nuts_example %>%
  mutate ( method = "actual")

#usethis::use_data (mixed_nuts_example, overwrite = TRUE)