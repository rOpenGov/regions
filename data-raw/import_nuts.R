require(readxl)
require(dplyr)
require(tidyr)

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
  dplyr::mutate_if(is.factor, as.character)

nuts_changes <- nuts_2021  %>%
  dplyr::select ( c("code_2016", "code_2021", 
                    "change_2021", "typology", "geo_name_2021")) %>%
  mutate ( code_2021 = ifelse ( is.na(code_2021), 
                                "discontinued in 2021", 
                                code_2021)) %>%
  mutate ( code_2016 = ifelse ( is.na(code_2016), 
                                "created in 2021", 
                                code_2016))


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
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( code_2016 = ifelse ( is.na(code_2016), 
                                "discontinued in 2016", 
                                code_2016)) %>%
  mutate ( code_2013 = ifelse ( is.na(code_2013), 
                                "created in 2016", 
                                code_2013))

nuts_changes <- nuts_2016 %>%
  dplyr::select ( c("code_2016", "code_2013", 
                    "change_2016", "typology", "geo_name_2016")) %>%
  full_join ( nuts_changes, by = c("code_2016", "typology") ) 

nuts_2013 <- read_excel(file.path("data-raw", "NUTS 2010 - NUTS 2013.xls"), 
                        sheet = "NUTS2010-NUTS2013", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2013") %>%
  dplyr::filter(!is.na(geo_name_2013)) %>%
  dplyr::rename( change_2013 = change ) %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( code_2013 = ifelse ( is.na(code_2013), 
                                "discontinued in 2013", 
                                code_2013)) %>%
  mutate ( code_2010 = ifelse ( is.na(code_2010), 
                                "created in 2013", 
                                code_2010))


unique ( nuts_changes$typology)
unique ( nuts_2013$typology)

nuts_changes <- nuts_2013 %>%
  dplyr::select ( c("code_2013", "code_2010", 
                    "change_2013", "typology", "geo_name_2013")) %>%
  full_join ( nuts_changes, by = c("code_2013", "typology") )


nuts_2010 <- read_excel(file.path("data-raw", "2006-2010.xls"), 
                        sheet = "NUTS2006-NUTS2010", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2010") %>%
  dplyr::filter(!is.na(geo_name_2010)) %>%
  dplyr::rename( change_2010 = change ) %>%
  dplyr::mutate_if(is.factor, as.character)  %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  mutate ( code_2010 = ifelse ( is.na(code_2010), 
                                "discontinued in 2010", 
                                code_2010)) %>%
  mutate ( code_2006 = ifelse ( is.na(code_2006), 
                                "created in 2010", 
                                code_2006))

nuts_changes <- nuts_2010 %>%
  dplyr::select ( c("code_2006", "code_2010", 
                    "change_2010", "typology", "geo_name_2010")) %>%
  full_join ( nuts_changes, by = c("code_2010", "typology"))

nuts_2006 <- read_excel(file.path("data-raw", "2003-2006.xls"), 
                        sheet = "NUTS2003-NUTS2006", skip = 1) %>%
  purrr::set_names(gsub(" ", "_", tolower(names(.)))) %>%
  purrr::set_names ( c('rowid', names(.)[2:ncol(.)])) %>%
  dplyr::select(1:12) %>%
  tidyr::pivot_longer ( cols = c( "country", "nuts_level_1",
                                  "nuts_level_2", "nuts_level_3" ), 
                        names_to = "typology", 
                        values_to = "geo_name_2006") %>%
  dplyr::filter(!is.na(geo_name_2006)) %>%
  dplyr::rename( change_2006 = change ) %>%
  mutate ( code_2006 = ifelse ( is.na(code_2006), 
                                "discontinued in 2006", 
                                code_2006)) %>%
  mutate ( code_2003 = ifelse ( is.na(code_2003), 
                                "created in 2006", 
                                code_2003))


nuts_changes <- nuts_2006 %>%
  dplyr::select ( c("code_2006", "code_2003", 
                    "change_2006", "typology", "geo_name_2006")) %>%
  full_join ( nuts_changes, by = c("code_2006", "typology"))

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
  mutate ( code_2003 = ifelse ( is.na(code_2003), 
                                "discontinued in 2003", 
                                code_2003)) %>%
  mutate ( code_1999 = ifelse ( is.na(code_1999), 
                                "created in 2003", 
                                code_1999))

nuts_changes <- nuts_2003 %>%
  dplyr::select ( c("code_1999", "code_2003", 
                    "change_2003", "typology", "geo_name_2003")) %>%
  full_join ( nuts_changes, by = c("code_2003", "typology"))

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
  mutate ( code_1999 = ifelse ( is.na(code_1999), 
                                "discontinued in 1999", 
                                code_1999)) %>%
  mutate ( code_1995 = ifelse ( is.na(code_1995), 
                                "created in 1999", 
                                code_1995))

nuts_changes <- nuts_1999 %>%
  dplyr::select ( c("code_1995", "code_1999", 
                    "change_1999", "typology", "geo_name_1999")) %>%
  full_join ( nuts_changes, by = c("code_1999", "typology"))

nuts_changes <- nuts_changes %>% 
  dplyr::select ( all_of(
  c('typology',
    'code_1995', 'code_1999', 'code_2003', 'code_2006', 
    'code_2010', 'code_2013', 'code_2016', 'code_2021', 
    'geo_name_1999', 'geo_name_2003', 'geo_name_2006', 'geo_name_2010',
    'geo_name_2013', 'geo_name_2016', 'geo_name_2021', 'change_1999',
    'change_2003', 'change_2006', 'change_2010', 
    'change_2013', 'change_2016', 'change_2021')
) ) %>%
  dplyr::arrange( typology, code_2016 )

saveRDS(nuts_changes, file.path("data-raw", "nuts_changes.rds"), 
        version = 2)




