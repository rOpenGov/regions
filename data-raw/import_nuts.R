require(readxl)
require(dplyr)
require(tidyr)
library(stringr)

nuts_2021 <- read_excel(file.path("data-raw", "NUTS2016-NUTS2021.xlsx"), 
                     sheet = "Changes detailed NUTS 2016-2021" ) %>%
  dplyr::select(1:12) %>%
  purrr::set_names(c("code_2016", "code_2021", "country", "nuts_level_1", 
                     "nuts_level_2", "nuts_level_3", "change_codes",
                     "change_2021", "nuts_level", "country_order",
                     "region_order", "region_order_21")
                   )  %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2021") %>%
  dplyr::filter(!is.na(geo_name_2021)) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_2021 %in% 
                                c("name change", "renamed", 
                                  "recoded and relabelled"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_2021, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_2021 %in% 
                                c("recode", "recoded", 
                                  "recoded (ressignment)", 
                                  "recoded and relabelled"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated", change_2021), 
                             TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( grepl("new|boundary|merge", change_2021), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse (is.na(code_2021), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 2021, NA_real_), 
           end_year = ifelse ( discontinued, 2016, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_2021) & other, TRUE, FALSE )) %>%
  select ( -problem, -other )


unique ( nuts_2021$change_2021)

all_valid_codes <- nuts_2021 %>%
  select ( typology, code_2021 ) %>%
  rename ( geo = code_2021 ) %>%
  mutate ( nuts = "code_2021" )


nuts_changes <- nuts_2021  %>%
  dplyr::select ( c("code_2016", "code_2021", 
                    "change_2021", "typology", "geo_name_2021", 
                   # "recoded", "relabelled", "new", "discontinued", 
                    "start_year", "end_year"))


nuts_2016 <- read_excel(file.path("data-raw", "NUTS2013-NUTS2016.xlsx"), 
                     sheet = "NUTS2013-NUTS2016", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2016") %>%
  dplyr::filter(!is.na(geo_name_2016)) %>%
  dplyr::rename( change_2016 = change ) %>%
  mutate ( change_2016 = str_trim(tolower(change_2016), side = 'both')) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_2016 %in% 
                                   c("name change", "renamed", 
                                     "recoded and relabelled", 
                                     "relabelled and recoded", 
                                     "relabelled", 
                                     "code, name change",
                                     "code change, name changes",
                                     "code change, name correction",
                                     "name change"), 
                                 TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_2016, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_2016 %in% 
                                c("recode", "recoded", "code change",
                                  "recoded and relabelled", 
                                  "relabelled and recoded",
                                  "recoded (ressignment)",  
                                  "code, name change",
                                  "code change, name changes",
                                  "code change, name correction"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated|merged|split",
                                         change_2016), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( is.na(code_2016),
                                   TRUE, discontinued)) %>%
  mutate ( new = ifelse ( grepl("new", change_2016), 
                          TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( change_2016 %in% c("boundary shift",
                                             "new region"), 
                          TRUE, new)
  ) %>%
  mutate ( new = ifelse (is.na(code_2016), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 2016, NA_real_), 
           end_year = ifelse ( discontinued, 2013, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded 
                            + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_2016) & other, 
                             TRUE, FALSE ))  %>%
  select ( -problem, -other )

all_valid_codes <- bind_rows ( 
  all_valid_codes, nuts_2016 %>%
  select ( typology, code_2016 ) %>%
  rename ( geo = code_2016 ) %>%
  mutate ( nuts = "code_2016" ))

sort(unique ( nuts_2016$change_2016))

nuts_changes <- nuts_2016 %>%
  dplyr::select ( c("code_2016", "code_2013", 
                    "change_2016", "typology", "geo_name_2016", 
                   # "recoded", "relabelled", "new", "discontinued", 
                    "start_year", "end_year")) %>%
  full_join ( nuts_changes, by = c("code_2016", "typology",
                                   #"recoded", 
                                 #"relabelled", "new", "discontinued", 
                                 "start_year", "end_year")) 

nuts_2013 <- read_excel(file.path("data-raw", 
                                  "NUTS 2010 - NUTS 2013.xls"), 
                        sheet = "NUTS2010-NUTS2013",
                        skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  mutate (country = ifelse (nchar(code_2013)>2,
                            NA_character_, country )) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2013") %>%
  dplyr::filter(!is.na(geo_name_2013)) %>%
  dplyr::rename( change_2013 = change ) %>%
  mutate ( change_2013 = str_trim(tolower(change_2013), side = 'both')) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_2013 %in% 
                                   c("name change", "renamed", 
                                     "recoded and relabelled", 
                                     "relabelled and recoded", 
                                     "relabelled", 
                                     "code, name change",
                                     "code change, name changes",
                                     "code change, name correction",
                                     "name change"), 
                                 TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_2013, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_2013 %in% 
                                c("recode", "recoded", "code change",
                                  "recoded and relabelled", 
                                  "relabelled and recoded",
                                  "recoded (ressignment)",  
                                  "code, name change",
                                  "code change, name changes",
                                  "code change, name correction"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated|merged|split",
                                         change_2013), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( is.na(code_2013),
                                   TRUE, discontinued)) %>%
  mutate ( new = ifelse ( grepl("new", change_2013), 
                          TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( change_2013 %in% c("boundary shift",
                                             "new region"), 
                          TRUE, new)
  ) %>%
  mutate ( new = ifelse (is.na(code_2013), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 2013, NA_real_), 
           end_year = ifelse ( discontinued, 2010, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded 
                            + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_2013) & other, 
                             TRUE, FALSE )) %>%
  select ( -problem, -other )


sort(unique ( nuts_2013$change_2013))
all_valid_codes <- bind_rows ( 
  all_valid_codes, nuts_2013 %>%
    select ( typology, code_2013 ) %>%
    rename ( geo = code_2013 ) %>%
    mutate ( nuts = "code_2013" ))

nuts_changes <- nuts_2013 %>%
  dplyr::select ( c("code_2013", "code_2010", 
                    "change_2013", "typology", "geo_name_2013", 
                  #"recoded", "relabelled", "new", "discontinued", 
                  "start_year", "end_year")) %>%
  full_join ( nuts_changes,
              c("code_2013", "typology", 
                #"recoded", "relabelled", 
                #"new", "discontinued", 
                "start_year", "end_year"))


nuts_2010 <- read_excel(file.path("data-raw", "2006-2010.xls"), 
                        sheet = "NUTS2006-NUTS2010", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  mutate (country = ifelse (nchar(code_2010)>2,
                            NA_character_, country )) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2010") %>%
  dplyr::filter(!is.na(geo_name_2010)) %>%
  dplyr::rename( change_2010 = change )  %>%
  mutate ( change_2010 = str_trim(tolower(change_2010), side = 'both')) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_2010 %in% 
                                   c("name change", "renamed", 
                                     "recoded and relabelled", 
                                     "relabelled and recoded", 
                                     "relabelled", 
                                     "Code, name change", 
                                     "Name change"), 
                                 TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_2010, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_2010 %in% 
                                c("recode", "recoded", "code change",
                                  "recoded and relabelled", 
                                  "relabelled and recoded",
                                  "recoded (ressignment)",  
                                  "code, name change",
                                  "code change, name change"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated|merged|split",
                                         change_2010), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( is.na(code_2010), TRUE, discontinued)) %>%
  mutate ( new = ifelse ( grepl("new", change_2010), 
                          TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( change_2010 %in% c("boundary shift",
                                             "new region"), 
                          TRUE, new)
  ) %>%
  mutate ( new = ifelse (is.na(code_2010), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 2010, NA_real_), 
           end_year = ifelse ( discontinued, 2006, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded 
                            + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_2010) & other, 
                             TRUE, FALSE )) %>%
  select ( -problem, -other )

all_valid_codes <- bind_rows ( 
  all_valid_codes, nuts_2010 %>%
    select ( typology, code_2010 ) %>%
    rename ( geo = code_2010 ) %>%
    mutate ( nuts = "code_2010" ))

nuts_changes <- nuts_2010 %>%
  dplyr::select ( c("code_2006", "code_2010", 
                    "change_2010", "typology", "geo_name_2010", 
                   # "recoded", "relabelled", "new", "discontinued", 
                    "start_year", "end_year")) %>%
  full_join ( nuts_changes, by = c("code_2010", "typology",
                                   #"recoded",
                                   #"relabelled", "new", "discontinued", 
                                   "start_year", "end_year"))

nuts_2006 <- read_excel(file.path("data-raw", "2003-2006.xls"), 
                        sheet = "NUTS2003-NUTS2006", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  mutate (country = ifelse (nchar(code_2006)>2,
                            NA_character_, country )) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2006") %>%
  dplyr::filter(!is.na(geo_name_2006)) %>%
  dplyr::rename( change_2006 = change ) %>%
  mutate ( change_2006 = str_trim(tolower(change_2006), side = 'both')) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_2006 %in% 
                                   c("name change", "renamed", 
                                     "recoded and relabelled", 
                                     "relabelled and recoded", 
                                     "relabelled", 
                                     "code, name change",
                                     "code change, name changes",
                                     "code change, name correction",
                                     "name change"), 
                                 TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_2006, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_2006 %in% 
                                c("recode", "recoded", "code change",
                                  "recoded and relabelled", 
                                  "relabelled and recoded",
                                  "recoded (ressignment)",  
                                  "code, name change",
                                  "code change, name changes",
                                  "code change, name correction"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated|merged|split",
                                         change_2006), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( is.na(code_2006), TRUE, discontinued)) %>%
  mutate ( new = ifelse ( grepl("new", change_2006), 
                          TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( change_2006 %in% c("boundary shift",
                                             "new region"), 
                          TRUE, new)
  ) %>%
  mutate ( new = ifelse (is.na(code_2006), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 2006, NA_real_), 
           end_year = ifelse ( discontinued, 2003, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded 
                            + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_2006) & other, 
                             TRUE, FALSE )) %>%
  select ( -problem, -other )

all_valid_codes <- bind_rows ( 
  all_valid_codes, nuts_2006 %>%
    select ( typology, code_2006 ) %>%
    rename ( geo = code_2006 ) %>%
    mutate ( nuts = "code_2006" ))

nuts_changes <- nuts_2006 %>%
  dplyr::select ( c("code_2006", "code_2003", 
                    "change_2006", "typology", "geo_name_2006", 
                    #"recoded", "relabelled", "new", "discontinued", 
                    "start_year", "end_year")) %>%
  full_join ( nuts_changes, 
              by = c("code_2006", "typology",
                     #"recoded", "relabelled", "new", "discontinued",
                     "start_year", "end_year"))

nuts_2003 <- read_excel(file.path("data-raw", "1999-2003.xls"), 
                        sheet = "NUTS1999-NUTS2003", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2003") %>%
  dplyr::filter(!is.na(geo_name_2003)) %>%
  dplyr::rename( change_2003 = change ) %>%
  mutate ( change_2003 = str_trim(tolower(change_2003), side = 'both')) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_2003 %in% 
                                   c("name change", "renamed", 
                                     "recoded and relabelled", 
                                     "relabelled and recoded", 
                                     "relabelled", 
                                     "code, name change",
                                     "code change, name changes",
                                     "code change, name correction",
                                     "name change"), 
                                 TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_2003, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_2003 %in% 
                                c("recode", "recoded", "code change",
                                  "recoded and relabelled", 
                                  "relabelled and recoded",
                                  "recoded (ressignment)",  
                                  "code, name change",
                                  "code change, name changes",
                                  "code change, name correction"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated|merged|split",
                                         change_2003), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( is.na(code_2003), TRUE, discontinued)) %>%
  mutate ( new = ifelse ( grepl("new", change_2003), 
                          TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( change_2003 %in% c("boundary shift",
                                             "new region"), 
                          TRUE, new)
  ) %>%
  mutate ( new = ifelse (is.na(code_2003), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 2003, NA_real_), 
           end_year = ifelse ( discontinued, 1999, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded 
                            + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_2003) & other, 
                             TRUE, FALSE )) %>%
  select ( -problem, -other )

nuts_changes <- nuts_2003 %>%
  dplyr::select ( c("code_1999", "code_2003", 
                    "change_2003", "typology", "geo_name_2003", 
                   # "recoded", "relabelled", "new", "discontinued", 
                    "start_year", "end_year")) %>%
  full_join ( nuts_changes, by = c("code_2003", "typology", 
                                   #"recoded",
                                   #"relabelled", "new", "discontinued", 
                                   "start_year", "end_year"))


all_valid_codes <- bind_rows ( 
  all_valid_codes, nuts_2003 %>%
    select ( typology, code_2003 ) %>%
    rename ( geo = code_2003 ) %>%
    mutate ( nuts = "code_2003" ))


nuts_1999 <- read_excel(file.path("data-raw", "1995-1999.xls"), 
                        sheet = "NUTS1995-NUTS1999", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_1999") %>%
  dplyr::filter(!is.na(geo_name_1999)) %>%
  dplyr::rename( change_1999 = change )  %>%
  mutate ( change_1999 = str_trim(tolower(change_1999), side = 'both')) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( relabelled = ifelse ( change_1999 %in% 
                                   c("name change", "renamed", 
                                     "recoded and relabelled", 
                                     "relabelled and recoded", 
                                     "relabelled", 
                                     "code, name change",
                                     "code change, name changes",
                                     "code change, name correction",
                                     "code and name change",
                                     "name change"), 
                                 TRUE, FALSE)
  ) %>%
  mutate ( relabelled = ifelse ( str_sub(change_1999, 1,6)=="rename", 
                                 TRUE, relabelled )
  ) %>%
  mutate ( recoded = ifelse ( change_1999 %in% 
                                c("recode", "recoded", "code change",
                                  "recoded and relabelled", 
                                  "relabelled and recoded",
                                  "recoded (ressignment)",  
                                  "code, name change",
                                  "code change, name changes",
                                  "code change, name correction", 
                                  "code and name change"), 
                              TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( grepl("discontinued|terminated|merged|split",
                                         change_1999), 
                                   TRUE, FALSE)
  ) %>%
  mutate ( discontinued = ifelse ( is.na(code_1999), TRUE, discontinued)) %>%
  mutate ( new = ifelse ( grepl("new", change_1999), 
                          TRUE, FALSE)
  ) %>%
  mutate ( new = ifelse ( change_1999 %in% c("boundary shift",
                                             "new region"), 
                          TRUE, new)
  ) %>%
  mutate ( new = ifelse (is.na(code_1999), FALSE, new )) %>%
  mutate ( start_year = ifelse ( new, 1999, NA_real_), 
           end_year = ifelse ( discontinued, 1995, NA_real_)) %>%
  mutate ( other = ifelse ( new + discontinued + recoded 
                            + relabelled == 0, 
                            TRUE, FALSE)) %>%
  mutate ( problem = ifelse (!is.na(change_1999) & other, 
                             TRUE, FALSE )) %>%
  select ( -problem, -other )


all_valid_codes <- bind_rows ( 
  all_valid_codes, nuts_1999 %>%
    select ( typology, code_1999 ) %>%
    rename ( geo = code_1999 ) %>%
    mutate ( nuts = "code_1999" ))

all_valid_nuts_codes <- all_valid_codes %>%
  filter ( !is.na(geo))


nuts_changes <- nuts_changes %>% 
  dplyr::select ( all_of(
  c('typology', 'start_year', 'end_year',
    'code_1999', 'code_2003', 'code_2006', 
    'code_2010', 'code_2013', 'code_2016', 'code_2021', 
   'geo_name_2003', 'geo_name_2006', 'geo_name_2010',
    'geo_name_2013', 'geo_name_2016', 'geo_name_2021', 
    'change_2003', 'change_2006', 'change_2010', 
    'change_2013', 'change_2016', 'change_2021')
) ) %>%
  dplyr::arrange( typology, code_2016 )

saveRDS(nuts_changes, file.path("data-raw", "nuts_changes.rds"), 
        version = 2)


usethis::use_data ( all_valid_nuts_codes , overwrite =TRUE )


