library(dplyr)
data ( "nuts_lau_2019" )

google_lau_matchtable <- nuts_lau_2019 %>%
  mutate ( country_code = case_when ( 
    country == "EL" ~ "GR", 
    country == "UK" ~ "GB", 
    TRUE ~ country )) %>%
  filter ( country_code %in% c("SI", "LV")) %>%
  select ( country_code, code_2016, lau_code,
           contains("name"), 
           -contains("change")) %>%
  mutate ( match_name = tolower(lau_name_latin ))

si_lau <- google_lau_matchtable %>%
  filter ( country_code == "SI") %>%
  select ( country_code, lau_code, code_2016, match_name ) %>%
  mutate ( match_name  = gsub(" ", "_", match_name ))

google_lau_names_si <- google_region_names %>%
  filter ( country_code %in% c("SI" ) ) %>%
  mutate ( match_name = case_when ( 
    country_code == "SI" & match_name == "izola" ~ "izola/isola", 
    country_code == "SI" & match_name == "koper" ~ "koper/capodistria",
    country_code == "SI" & match_name == "lendava" ~ "lendava/lendva",
    country_code == "SI" & match_name == "piran" ~ "piran/pirano",
    country_code == "SI" & match_name == "administrative_unit_maribor" ~ "maribor", 
    country_code == "SI" & match_name == "municipality_of_hrastnik" ~ "hrastnik",
    TRUE ~ match_name )) %>%
  select ( -code_2016 ) %>%
  left_join ( si_lau,  by = c("country_code", "match_name") ) %>%
  mutate ( code_2016 = ifelse(match_name == "slovenia", "SI", code_2016))


lv_lau <- google_lau_matchtable %>%
  filter ( country_code == "LV") %>%
  select ( country_code, lau_name_national,
           lau_code, code_2016, match_name ) %>%
  mutate ( match_name = tolower(lau_name_national) ) %>%
  mutate ( match_name  = gsub(" ", "_", match_name )) %>%
  mutate ( match_name = gsub ("_novads", "_municipality", match_name)) %>%
  select (-lau_name_national )

google_lau_names_lv <- google_region_names %>%
  filter ( country_code %in% c("LV") ) %>%
  mutate ( match_name = case_when ( 
    country_code == "LV" & match_name == "ādaži_municipality" ~ "ādažu_municipality", 
    country_code == "LV" & match_name == "aizkraukle_municipality" ~ "aizkraukles_municipality",
    country_code == "LV" & match_name == "aloja_municipality" ~ "alojas_municipality",
    country_code == "LV" & match_name == "alūksne_municipality" ~ "alūksnes_municipality",
    country_code == "LV" & grepl( "ksne_municipality", match_name)  ~ gsub("_municipality", "s_municipality", match_name),
    country_code == "LV" & match_name == "bauska_municipality" ~ "bauskas_municipality", 
    country_code == "LV" & match_name == "baldone_municipality" ~ "baldones_municipality", 
    country_code == "LV" & match_name == "balvi_municipality" ~ "balvu_municipality", 
    country_code == "LV" & match_name == "burtnieki_municipality" ~ "burtnieku_municipality", 
    country_code == "LV" & match_name == "carnikava_municipality" ~ "carnikavas_municipality",
    country_code == "LV" & match_name == "cēsis_municipality" ~ "cēsu_municipality",
    country_code == "LV" & match_name == "dobele_municipality" ~ "dobeles_municipality",
    country_code == "LV" & match_name == "engure_municipality" ~ "engures_municipality",
    country_code == "LV" & match_name == "garkalne_municipality" ~ "garkalnes_municipality",
    country_code == "LV" & match_name == "iecava_municipality" ~ "iecavas_municipality",
    country_code == "LV" & match_name == "jelgava_municipality" ~ "jelgavas_municipality",
    country_code == "LV" & grepl( "ekava_municipality", match_name)  ~ gsub("_municipality", "s_municipality", match_name),
    country_code == "LV" & grepl( "ile_municipality", match_name)  ~ gsub("_municipality", "s_municipality", match_name),
    country_code == "LV" & grepl( "slava_municipality", match_name)  ~ gsub("_municipality", "s_municipality", match_name),
    country_code == "LV" & grepl( "ukalns_municipality", match_name)  ~ gsub("s_municipality", "a_municipality", match_name),
    country_code == "LV" & grepl( "gums_municipality", match_name)  ~ gsub("s_municipality", "a_municipality", match_name),
    country_code == "LV" & match_name == "krimulda_municipality" ~ "krimuldas_municipality",
    country_code == "LV" & match_name == "lielvārde_municipality" ~ "lielvārdes_municipality",
    country_code == "LV" & match_name == "saulkrasti_municipality" ~ "saulkrastu_municipality",
    country_code == "LV" & match_name == "riga" ~ "rīga",
    country_code == "LV" & match_name == "smiltene_municipality" ~ "smiltenes_municipality",
    country_code == "LV" & match_name == "talsi_municipality" ~ "talsu_municipality",
    country_code == "LV" & match_name == "tukums_municipality" ~ "tukuma_municipality",
    country_code == "LV" & grepl( "rde_municipality", match_name)  ~ gsub("_municipality", "s_municipality", match_name),
    country_code == "LV" & grepl( "rupe_municipality", match_name)  ~ gsub("_municipality", "s_municipality", match_name),
    country_code == "LV" & grepl( "i_municipality", match_name)  ~ gsub("i_municipality", "u_municipality", match_name),
    country_code == "LV" & grepl( "a_municipality", match_name)  ~ gsub("a_municipality", "as_municipality", match_name),
    country_code == "LV" & grepl( "kne_municipality", match_name)  ~ gsub("kne_municipality", "knes_municipality", match_name),
    country_code == "LV" & grepl( "gre|ne|te_municipality", match_name)  ~ gsub("e_municipality", "es_municipality", match_name),
    country_code == "LV" & grepl( "city_of_liep", match_name)  ~ gsub("city_of_", "", match_name),
    country_code == "LV" & grepl( "sis", match_name)  ~ gsub("sis_municipality", "su_municipality", match_name),
    
    TRUE ~ match_name )) %>%
  select ( -code_2016 ) %>%
  left_join ( lv_lau,  by = c("country_code", "match_name") ) %>%
  mutate ( code_2016 = ifelse(match_name == "latvia",
                              "LV", code_2016)) %>%
  mutate ( lau_code = case_when (
    country_code == "LV" & google_name == "riga" ~"0010000", 
     TRUE ~ lau_code)) %>%
  mutate ( code_2016 = case_when (
    country_code == "LV" & google_name == "riga" ~"LV006", 
    TRUE ~ code_2016))
  
google_lau_si_lv <- google_lau_names_lv %>%
  dplyr::bind_rows(google_lau_names_si) %>%
  tidyr::unite (., col = "code_2016", c("code_2016", "lau_code") ) %>%
  mutate ( code_2016 = case_when (
    code_2016 == "LV_NA" ~ "LV", 
    code_2016 == "SI_NA" ~ "SI", 
    TRUE ~ code_2016
  ))
  
rm(google_lau_matchtable,google_lau_names_lv, google_lau_names_si )

## Will not work with Portugal
