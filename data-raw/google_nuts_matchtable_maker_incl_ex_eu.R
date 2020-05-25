library(regions)
library(tidyverse)
#' @author István Zsoldos
#' 

normalize_text <- function(x) {
  
  x <- as.character(x)
  x <- tolower(x)
  x <- str_trim(x, side = "both")
  x <- gsub("\\s", "_", x)
  x <- gsub(":|!|,|;|%","", x)
  x <- gsub("____|___|__", "_", x)
  x
}

### use your own path or save a copy of the Global Mobility Reports
###  into 'data-raw'
###  This file is not synchronized with the GitHub Repo because it
### is large and will slow down gits.

gmr_csv <- read_csv( file.path('data-raw', 
                               'Global_Mobility_Report.csv'))

gmr <- gmr_csv %>%
  set_names ( c("country_code", "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2", 
                "date", "retail", "grocery", 
                "parks", "transit", "workplaces", "residential") ) 

## First joining with valid NUTS codes ------------
## When there is no region name, use the country name
## And preferably the English country name, not the national language one

data("all_valid_nuts_codes", package = 'regions')

all_valid_nuts_codes 



nuts_gmr <- all_valid_nuts_codes %>%
  mutate ( country_code = get_country_code(geo)) %>%
  distinct ( country_code ) %>% 
  left_join ( gmr, 
              by = 'country_code' ) %>%
  mutate ( google_region_name_1 = case_when (
    is.na(google_region_name_1) & is.na(google_region_name_2) ~ google_country_name, 
    TRUE ~ google_region_name_1
  )) %>%
  pivot_longer ( cols = c("google_region_name_1", 
                          "google_region_name_2"), 
                 names_to = "google_region_level", 
                 values_to = "google_region_name") %>%
  mutate ( google_name = normalize_text (google_region_name )) %>%
  filter ( !is.na(google_name))

## Help table ------------------------------------------------
## Current regions and their official names in NUTS2016, 
## recent changes, name variants in NUTS2013 and NUTS2010 
## for countries where only nuts 2021 is available, ituses that

data("nuts_changes", package = 'regions')

only_2021_nuts <- all_valid_nuts_codes %>%
  mutate ( country_code = get_country_code(geo)) %>%
  select (country_code, nuts, typology) %>%
  filter( nuts %in% c("code_2016", "code_2021") & typology == "country") %>% 
  select ( -all_of ("typology")) %>%
  unique () %>% 
  mutate( count = 1 ) %>%
  pivot_wider( names_from = "nuts",
               values_from = "count") %>%
  filter( is.na(code_2016) ) %>%
  select (country_code) %>% unlist() %>% unname()

regions_and_names_2016 <- all_valid_nuts_codes %>%
  mutate ( country_code = get_country_code(geo)) %>%
  filter ( nuts == "code_2016" | country_code %in% only_2021_nuts) %>%
  rename ( code_2016 = geo ) %>% 
  left_join ( nuts_changes %>%
                select ( typology, code_2016, 
                         geo_name_2021, geo_name_2016, geo_name_2013, 
                         geo_name_2010, change_2016), 
              by = c('typology', 'code_2016')) %>%
  mutate ( country_name = countrycode::countrycode(country_code, 
                                                   "iso2c", 'country.name')
  ) %>%
  mutate ( match_name  = case_when(
    typology == "country" ~ normalize_text(country_name) ,
    nuts == "code_2021" ~ normalize_text(geo_name_2021),
    TRUE ~ normalize_text(geo_name_2016)   )
  )

## Google region names before national corrections ------------
google_region_names <- nuts_gmr %>%
  select ( country_code, google_region_level,
           google_region_name, google_name ) %>%
  filter ( complete.cases(.)) %>%
  distinct_all() %>% 
  mutate ( match_name = google_name )

##  What was found at first try  ---------------------------------

found_in_nuts_distinct <- google_region_names %>%
  left_join ( regions_and_names_2016 , 
              by = c("country_code", "match_name"))

## Making google_region_names$match_name equal to regions_and_names_2016$match_name when there is a 1-to-1 correspondence

#some general changes for better fit
#switching for local county name in SE
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse((country_code == "SE" & grepl( "_county", match_name)), gsub("_county", "s_län", match_name), match_name)) 

#getting rid of "_county" from names in RO
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse((country_code == "RO" & grepl( "_county", match_name)), gsub("_county", "", match_name), match_name)) 

# changing some names in HU
hungary_names <- regions_and_names_2016 %>%
  filter ( country_code  == "HU") %>%
  select ( match_name)  %>%
  unlist() %>% as.character() %>% sort()

google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "HU" & grepl("moson-sopron", match_name) ~  hungary_names[grepl("moson-sopron", hungary_names)],
    country_code == "HU" & grepl( "hungary|budapest", match_name) ~ match_name,
    country_code == "HU" & grepl( "_county", match_name) ~ gsub("_county", "", match_name), 
    TRUE ~ match_name))


# Adding code_2016 values where match is possible
google_region_names <- google_region_names %>%
  left_join ( regions_and_names_2016 %>% select (c(country_code, code_2016, match_name)) , 
              by = c("country_code", "match_name"))

## Fixing Belgium

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "BE" & match_name == "brussels" ~ "BE1",
    country_code == "BE" & match_name == "flanders" ~ "BE2",
    country_code == "BE" & match_name == "wallonia" ~ "BE3",
    TRUE ~ code_2016))

#changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "BE" & match_name == "brussels" ~ "région_de_bruxelles-capitale/brussels_hoofdstedelijk_gewest",
    country_code == "BE" & match_name == "flanders" ~ "vlaams_gewest",
    country_code == "BE" & match_name == "wallonia" ~ "région_wallonne",
    TRUE ~ match_name))


## Fixing Bulgaria
# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "BG" & match_name == "blagoevgrad_province" ~ "BG413",
    country_code == "BG" & match_name == "burgas" ~ "BG341",
    country_code == "BG" & match_name == "dobrich_province" ~ "BG332",
    country_code == "BG" & match_name == "gabrovo" ~ "BG322",
    country_code == "BG" & match_name == "haskovo_province" ~ "BG422",
    country_code == "BG" & match_name == "jambol" ~ "BG343",
    country_code == "BG" & match_name == "kardzhali_province" ~ "BG425",
    country_code == "BG" & match_name == "kyustendil_province" ~ "BG415",
    country_code == "BG" & match_name == "lovec" ~ "BG315",
    country_code == "BG" & match_name == "montana_province" ~ "BG312",
    country_code == "BG" & match_name == "pazardzhik" ~ "BG423",
    country_code == "BG" & match_name == "pernik" ~ "BG414",
    country_code == "BG" & match_name == "pleven_province" ~ "BG314",
    country_code == "BG" & match_name == "plovdiv_province" ~ "BG421",
    country_code == "BG" & match_name == "razgrad" ~ "BG324",
    country_code == "BG" & match_name == "ruse" ~ "BG323",
    country_code == "BG" & match_name == "shumen_province" ~ "BG333",
    country_code == "BG" & match_name == "silistra" ~ "BG325",
    country_code == "BG" & match_name == "sliven_province" ~ "BG342",
    country_code == "BG" & match_name == "smoljan" ~ "BG424",
    country_code == "BG" & match_name == "sofia_city_province" ~ "BG411",
    country_code == "BG" & match_name == "sofia_province" ~ "BG412",
    country_code == "BG" & match_name == "stara_zagora" ~ "BG344",
    country_code == "BG" & match_name == "targovishte_province" ~ "BG334",
    country_code == "BG" & match_name == "varna" ~ "BG331",
    country_code == "BG" & match_name == "veliko_tarnovo_province" ~ "BG321",
    country_code == "BG" & match_name == "vidin" ~ "BG311",
    country_code == "BG" & match_name == "vraca" ~ "BG313",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "BG" & match_name == "blagoevgrad_province" ~ "благоевград",
    country_code == "BG" & match_name == "burgas" ~ "бургас",
    country_code == "BG" & match_name == "dobrich_province" ~ "добрич",
    country_code == "BG" & match_name == "gabrovo" ~ "габрово",
    country_code == "BG" & match_name == "haskovo_province" ~ "хасково",
    country_code == "BG" & match_name == "jambol" ~ "ямбол",
    country_code == "BG" & match_name == "kardzhali_province" ~ "кърджали",
    country_code == "BG" & match_name == "kyustendil_province" ~ "кюстендил",
    country_code == "BG" & match_name == "lovec" ~ "ловеч",
    country_code == "BG" & match_name == "montana_province" ~ "монтана",
    country_code == "BG" & match_name == "pazardzhik" ~ "пазарджик",
    country_code == "BG" & match_name == "pernik" ~ "перник",
    country_code == "BG" & match_name == "pleven_province" ~ "плевен",
    country_code == "BG" & match_name == "plovdiv_province" ~ "пловдив",
    country_code == "BG" & match_name == "razgrad" ~ "разград",
    country_code == "BG" & match_name == "ruse" ~ "русе",
    country_code == "BG" & match_name == "shumen_province" ~ "шумен",
    country_code == "BG" & match_name == "silistra" ~ "силистра",
    country_code == "BG" & match_name == "sliven_province" ~ "сливен",
    country_code == "BG" & match_name == "smoljan" ~ "смолян",
    country_code == "BG" & match_name == "sofia_city_province" ~ "софия_(столица)",
    country_code == "BG" & match_name == "sofia_province" ~ "софия",
    country_code == "BG" & match_name == "stara_zagora" ~ "стара_загора",
    country_code == "BG" & match_name == "targovishte_province" ~ "търговище",
    country_code == "BG" & match_name == "varna" ~ "варна",
    country_code == "BG" & match_name == "veliko_tarnovo_province" ~ "велико_търново",
    country_code == "BG" & match_name == "vidin" ~ "видин",
    country_code == "BG" & match_name == "vraca" ~ "враца",
    TRUE ~ match_name))


## Fixing Czechia

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "CZ" & match_name == "central_bohemian_region" ~ "CZ020",
    country_code == "CZ" & match_name == "hradec_králové_region" ~ "CZ052",
    country_code == "CZ" & match_name == "karlovy_vary_region" ~ "CZ041",
    country_code == "CZ" & match_name == "liberec_region" ~ "CZ051",
    country_code == "CZ" & match_name == "moravian-silesian_region" ~ "CZ080",
    country_code == "CZ" & match_name == "olomouc_region" ~ "CZ071",
    country_code == "CZ" & match_name == "pardubice_region" ~ "CZ053",
    country_code == "CZ" & match_name == "plzeň_region" ~ "CZ032",
    country_code == "CZ" & match_name == "prague" ~ "CZ010",
    country_code == "CZ" & match_name == "south_bohemian_region" ~ "CZ031",
    country_code == "CZ" & match_name == "south_moravian_region" ~ "CZ064",
    country_code == "CZ" & match_name == "ústí_nad_labem_region" ~ "CZ042",
    country_code == "CZ" & match_name == "vysocina_region" ~ "CZ063",
    country_code == "CZ" & match_name == "zlin_region" ~ "CZ072",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "CZ" & match_name == "central_bohemian_region" ~ "středočeský_kraj",
    country_code == "CZ" & match_name == "hradec_králové_region" ~ "královéhradecký_kraj",
    country_code == "CZ" & match_name == "karlovy_vary_region" ~ "karlovarský_kraj",
    country_code == "CZ" & match_name == "liberec_region" ~ "liberecký_kraj",
    country_code == "CZ" & match_name == "moravian-silesian_region" ~ "moravskoslezský_kraj",
    country_code == "CZ" & match_name == "olomouc_region" ~ "olomoucký_kraj",
    country_code == "CZ" & match_name == "pardubice_region" ~ "pardubický_kraj",
    country_code == "CZ" & match_name == "plzeň_region" ~ "plzeňský_kraj",
    country_code == "CZ" & match_name == "prague" ~ "hlavní_město_praha",
    country_code == "CZ" & match_name == "south_bohemian_region" ~ "jihočeský_kraj",
    country_code == "CZ" & match_name == "south_moravian_region" ~ "jihomoravský_kraj",
    country_code == "CZ" & match_name == "ústí_nad_labem_region" ~ "ústecký_kraj",
    country_code == "CZ" & match_name == "vysocina_region" ~ "kraj_vysočina",
    country_code == "CZ" & match_name == "zlin_region" ~ "zlínský_kraj",
    TRUE ~ match_name))

#additional fixing for plzen
google_region_names <- google_region_names %>% mutate( code_2016 = ifelse( country_code == "CZ" & grepl( "plze", match_name), "CZ032", code_2016))
google_region_names <- google_region_names %>% mutate( match_name = ifelse( country_code == "CZ" & grepl( "plze", match_name), "plzeňský_kraj", match_name))


# Fixing Denmark

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "DK" & match_name == "capital_region_of_denmark" ~ "DK01",
    country_code == "DK" & match_name == "central_denmark_region" ~ "DK04",
    country_code == "DK" & match_name == "north_denmark_region" ~ "DK05",
    country_code == "DK" & match_name == "region_of_southern_denmark" ~ "DK03",
    country_code == "DK" & match_name == "region_zealand" ~ "DK02",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "DK" & match_name == "capital_region_of_denmark" ~ "hovedstaden",
    country_code == "DK" & match_name == "central_denmark_region" ~ "midtjylland",
    country_code == "DK" & match_name == "north_denmark_region" ~ "nordjylland",
    country_code == "DK" & match_name == "region_of_southern_denmark" ~ "syddanmark",
    country_code == "DK" & match_name == "region_zealand" ~ "sjælland",
    TRUE ~ match_name))


# Fixing Germany

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "DE" & match_name == "bavaria" ~ "DE2",
    country_code == "DE" & match_name == "hesse" ~ "DE7",
    country_code == "DE" & match_name == "lower_saxony" ~ "DE9",
    country_code == "DE" & match_name == "north_rhine-westphalia" ~ "DEA",
    country_code == "DE" & match_name == "rhineland-palatinate" ~ "DEB",
    country_code == "DE" & match_name == "saxony" ~ "DED",
    country_code == "DE" & match_name == "saxony-anhalt" ~ "DEE",
    country_code == "DE" & match_name == "thuringia" ~ "DEG0",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "DE" & match_name == "bavaria" ~ "bayern",
    country_code == "DE" & match_name == "hesse" ~ "hessen",
    country_code == "DE" & match_name == "lower_saxony" ~ "niedersachsen",
    country_code == "DE" & match_name == "north_rhine-westphalia" ~ "nordrhein-westfalen",
    country_code == "DE" & match_name == "rhineland-palatinate" ~ "rheinland-pfalz",
    country_code == "DE" & match_name == "saxony" ~ "sachsen",
    country_code == "DE" & match_name == "saxony-anhalt" ~ "sachsen-anhalt",
    country_code == "DE" & match_name == "thuringia" ~ "thüringen",
    TRUE ~ match_name))


# Fixing Estonia (Local government level data only, skipping for the moment, only deleting "_county")

google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse((country_code == "EE" & grepl( "_county", match_name)), gsub("_county", "", match_name), match_name)) 


# Fixing Ireland (Local government level data only, only Dublin County corresponds to NUTS3, skipping the rest for the moment)

# changing nuts code
google_region_names <- google_region_names %>%
  mutate ( code_2016 = ifelse(country_code == "IE" & match_name == "county_dublin", "IE061", code_2016)) 

# changing name
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse(country_code == "IE" & match_name == "county_dublin", "dublin", match_name)) 


# Fixing Greece ("Decentralized Administration", regions are made up of one or more NUTS2 regions, no change for the moment)


# Fixing Spain

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "ES" & match_name == "andalusia" ~ "ES61",
    country_code == "ES" & match_name == "aragon" ~ "ES24",
    country_code == "ES" & match_name == "balearic_islands" ~ "ES53",
    country_code == "ES" & match_name == "basque_country" ~ "ES21",
    country_code == "ES" & match_name == "canary_islands" ~ "ES70",
    country_code == "ES" & match_name == "castile_and_león" ~ "ES41",
    country_code == "ES" & match_name == "castile-la_mancha" ~ "ES42",
    country_code == "ES" & match_name == "catalonia" ~ "ES51",
    country_code == "ES" & match_name == "ceuta" ~ "ES63",
    country_code == "ES" & match_name == "community_of_madrid" ~ "ES30",
    country_code == "ES" & match_name == "melilla" ~ "ES64",
    country_code == "ES" & match_name == "navarre" ~ "ES22",
    country_code == "ES" & match_name == "region_of_murcia" ~ "ES62",
    country_code == "ES" & match_name == "valencian_community" ~ "ES52",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "ES" & match_name == "andalusia" ~ "andalucía",
    country_code == "ES" & match_name == "aragon" ~ "aragón",
    country_code == "ES" & match_name == "balearic_islands" ~ "illes_balears",
    country_code == "ES" & match_name == "basque_country" ~ "país_vasco",
    country_code == "ES" & match_name == "canary_islands" ~ "canarias",
    country_code == "ES" & match_name == "castile_and_león" ~ "castilla_y_león",
    country_code == "ES" & match_name == "castile-la_mancha" ~ "castilla-la_mancha",
    country_code == "ES" & match_name == "catalonia" ~ "cataluña",
    country_code == "ES" & match_name == "ceuta" ~ "ciudad_autónoma_de_ceuta",
    country_code == "ES" & match_name == "community_of_madrid" ~ "comunidad_de_madrid",
    country_code == "ES" & match_name == "melilla" ~ "ciudad_autónoma_de_melilla",
    country_code == "ES" & match_name == "navarre" ~ "comunidad_foral_de_navarra",
    country_code == "ES" & match_name == "region_of_murcia" ~ "región_de_murcia",
    country_code == "ES" & match_name == "valencian_community" ~ "comunidad_valenciana",
    TRUE ~ match_name))


#Fixing France

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "FR" & match_name == "brittany" ~ "FRH",
    country_code == "FR" & match_name == "centre-val_de_loire" ~ "FRB",
    country_code == "FR" & match_name == "corsica" ~ "FRM",
    country_code == "FR" & match_name == "grand_est" ~ "FRF",
    country_code == "FR" & match_name == "hauts-de-france" ~ "FRE",
    country_code == "FR" & match_name == "île-de-france" ~ "FR1",
    country_code == "FR" & match_name == "normandy" ~ "FRD",
    country_code == "FR" & match_name == "nouvelle-aquitaine" ~ "FRI",
    country_code == "FR" & match_name == "occitanie" ~ "FRJ",
    country_code == "FR" & match_name == "provence-alpes-côte_d'azur" ~ "FRL",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "FR" & match_name == "brittany" ~ "bretagne",
    country_code == "FR" & match_name == "centre-val_de_loire" ~ "centre_—_val_de_loire",
    country_code == "FR" & match_name == "corsica" ~ "corse",
    country_code == "FR" & match_name == "grand_est" ~ "alsace-champagne-ardenne-lorraine",
    country_code == "FR" & match_name == "hauts-de-france" ~ "nord-pas_de_calais-picardie",
    country_code == "FR" & match_name == "île-de-france" ~ "ile-de-france",
    country_code == "FR" & match_name == "normandy" ~ "normandie",
    country_code == "FR" & match_name == "nouvelle-aquitaine" ~ "aquitaine-limousin-poitou-charentes",
    country_code == "FR" & match_name == "occitanie" ~ "languedoc-roussillon-midi-pyrénées",
    country_code == "FR" & match_name == "provence-alpes-côte_d'azur" ~ "provence-alpes-côte_d’azur",
    TRUE ~ match_name))


# Fixing Croatia

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "HR" & match_name == "bjelovar-bilogora_county" ~ "HR047",
    country_code == "HR" & match_name == "brod-posavina_county" ~ "HR04A",
    country_code == "HR" & match_name == "city_of_zagreb" ~ "HR041",
    country_code == "HR" & match_name == "dubrovnik-neretva_county" ~ "HR037",
    country_code == "HR" & match_name == "istria_county" ~ "HR036",
    country_code == "HR" & match_name == "karlovac_county" ~ "HR04D",
    country_code == "HR" & match_name == "koprivnica-križevci_county" ~ "HR045",
    country_code == "HR" & match_name == "krapina-zagorje_county" ~ "HR043",
    country_code == "HR" & match_name == "lika-senj_county" ~ "HR032",
    country_code == "HR" & match_name == "međimurje_county" ~ "HR046",
    country_code == "HR" & match_name == "osijek-baranja_county" ~ "HR04B",
    country_code == "HR" & match_name == "požega-slavonia_county" ~ "HR049",
    country_code == "HR" & match_name == "primorje-gorski_kotar_county" ~ "HR031",
    country_code == "HR" & match_name == "šibenik-knin_county" ~ "HR034",
    country_code == "HR" & match_name == "sisak-moslavina_county" ~ "HR04E",
    country_code == "HR" & match_name == "split-dalmatia_county" ~ "HR035",
    country_code == "HR" & match_name == "varaždin_county" ~ "HR044",
    country_code == "HR" & match_name == "virovitica-podravina_county" ~ "HR048",
    country_code == "HR" & match_name == "vukovar-srijem_county" ~ "HR04C",
    country_code == "HR" & match_name == "zadar_county" ~ "HR033",
    country_code == "HR" & match_name == "zagreb_county" ~ "HR042",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "HR" & match_name == "bjelovar-bilogora_county" ~ "bjelovarsko-bilogorska_županija",
    country_code == "HR" & match_name == "brod-posavina_county" ~ "brodsko-posavska_županija",
    country_code == "HR" & match_name == "city_of_zagreb" ~ "grad_zagreb",
    country_code == "HR" & match_name == "dubrovnik-neretva_county" ~ "dubrovačko-neretvanska_županija",
    country_code == "HR" & match_name == "istria_county" ~ "istarska_županija",
    country_code == "HR" & match_name == "karlovac_county" ~ "karlovačka_županija",
    country_code == "HR" & match_name == "koprivnica-križevci_county" ~ "koprivničko-križevačka_županija",
    country_code == "HR" & match_name == "krapina-zagorje_county" ~ "krapinsko-zagorska_županija",
    country_code == "HR" & match_name == "lika-senj_county" ~ "ličko-senjska_županija",
    country_code == "HR" & match_name == "međimurje_county" ~ "međimurska_županija",
    country_code == "HR" & match_name == "osijek-baranja_county" ~ "osječko-baranjska_županija",
    country_code == "HR" & match_name == "požega-slavonia_county" ~ "požeško-slavonska_županija",
    country_code == "HR" & match_name == "primorje-gorski_kotar_county" ~ "primorsko-goranska_županija",
    country_code == "HR" & match_name == "šibenik-knin_county" ~ "HR034",
    country_code == "HR" & match_name == "sisak-moslavina_county" ~ "sisačko-moslavačka_županija",
    country_code == "HR" & match_name == "split-dalmatia_county" ~ "splitsko-dalmatinska_županija",
    country_code == "HR" & match_name == "varaždin_county" ~ "varaždinska_županija",
    country_code == "HR" & match_name == "virovitica-podravina_county" ~ "virovitičko-podravska_županija",
    country_code == "HR" & match_name == "vukovar-srijem_county" ~ "vukovarsko-srijemska_županija",
    country_code == "HR" & match_name == "zadar_county" ~ "zadarska_županija",
    country_code == "HR" & match_name == "zagreb_county" ~ "zagrebačka_županija",
    TRUE ~ match_name))


#additional fixing for međimurje_county
google_region_names <- google_region_names %>%
  mutate( code_2016 = ifelse( country_code == "HR" & grepl( "imurje_county", match_name), "HR046", code_2016))
google_region_names <- google_region_names %>%
  mutate( match_name = ifelse( country_code == "HR" & grepl( "imurje_county", match_name), "međimurska_županija", match_name))


# Fixing Italy. The "trentino-south_tyrol" region is made up of two nuts regions:
# the "provincia_autonoma_di_bolzano/bozen" and the "provincia_autonoma_di_trento" regions (ITH1 and 	ITH2 respectively)
# "trentino-south_tyrol" was left unchanged for the moment

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "IT" & match_name == "aosta" ~ "ITC2",
    country_code == "IT" & match_name == "apulia" ~ "ITF4",
    country_code == "IT" & match_name == "lombardy" ~ "ITC4",
    country_code == "IT" & match_name == "piedmont" ~ "ITC1",
    country_code == "IT" & match_name == "sardinia" ~ "ITG2",
    country_code == "IT" & match_name == "sicily" ~ "ITG1",
    country_code == "IT" & match_name == "tuscany" ~ "ITI1",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "IT" & match_name == "aosta" ~ "valle_d’aosta/vallée_d’aoste",
    country_code == "IT" & match_name == "apulia" ~ "puglia",
    country_code == "IT" & match_name == "lombardy" ~ "lombardia",
    country_code == "IT" & match_name == "piedmont" ~ "piemonte",
    country_code == "IT" & match_name == "sardinia" ~ "sardegna",
    country_code == "IT" & match_name == "sicily" ~ "sicilia",
    country_code == "IT" & match_name == "tuscany" ~ "toscana",
    TRUE ~ match_name))


# Fixing Latvia (Municipal data only, no change for the moment)


# Fixing Lithuania (chaning "_county" to "_apskritis" in name, this should match nuts3 regions)

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "LT" & match_name == "alytus_county" ~ "LT021",
    country_code == "LT" & match_name == "kaunas_county" ~ "LT022",
    country_code == "LT" & match_name == "klaipėda_county" ~ "LT023",
    country_code == "LT" & match_name == "marijampolė_county" ~ "LT024",
    country_code == "LT" & match_name == "panevėžys_county" ~ "LT025",
    country_code == "LT" & match_name == "šiauliai_county" ~ "LT026",
    country_code == "LT" & match_name == "tauragė_county" ~ "LT027",
    country_code == "LT" & match_name == "telšiai_county" ~ "LT028",
    country_code == "LT" & match_name == "utena_county" ~ "LT029",
    country_code == "LT" & match_name == "vilnius_county" ~ "LT011",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "LT" & match_name == "alytus_county" ~ "alytaus_apskritis",
    country_code == "LT" & match_name == "kaunas_county" ~ "kauno_apskritis",
    country_code == "LT" & match_name == "klaipėda_county" ~ "klaipėdos_apskritis",
    country_code == "LT" & match_name == "marijampolė_county" ~ "marijampolės_apskritis",
    country_code == "LT" & match_name == "panevėžys_county" ~ "panevėžio_apskritis",
    country_code == "LT" & match_name == "šiauliai_county" ~ "šiaulių_apskritis",
    country_code == "LT" & match_name == "tauragė_county" ~ "tauragės_apskritis",
    country_code == "LT" & match_name == "telšiai_county" ~ "telšių_apskritis",
    country_code == "LT" & match_name == "utena_county" ~ "utenos_apskritis",
    country_code == "LT" & match_name == "vilnius_county" ~ "vilniaus_apskritis",
    TRUE ~ match_name))


# Fixing 	Luxembourg (no change, one region in google too)


# Fixing Hungary (names were changed at the beginning)


# Fixing 	Malta (no change, one region in google too)


# Fixing the Netherlands

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "NL" & match_name == "friesland" ~ "NL12",
    country_code == "NL" & match_name == "limburg" ~ "NL42",
    country_code == "NL" & match_name == "north_brabant" ~ "NL41",
    country_code == "NL" & match_name == "north_holland" ~ "NL32",
    country_code == "NL" & match_name == "south_holland" ~ "NL33",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "NL" & match_name == "friesland" ~ "friesland_(nl)",
    country_code == "NL" & match_name == "limburg" ~ "limburg_(nl)",
    country_code == "NL" & match_name == "north_brabant" ~ "noord-brabant",
    country_code == "NL" & match_name == "north_holland" ~ "noord-holland",
    country_code == "NL" & match_name == "south_holland" ~ "zuid-holland",
    TRUE ~ match_name))


# Fixing Austria

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "AT" & match_name == "carinthia" ~ "AT21",
    country_code == "AT" & match_name == "lower_austria" ~ "AT12",
    country_code == "AT" & match_name == "styria" ~ "AT22",
    country_code == "AT" & match_name == "tyrol" ~ "AT33",
    country_code == "AT" & match_name == "upper_austria" ~ "AT31",
    country_code == "AT" & match_name == "vienna" ~ "AT13",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "AT" & match_name == "carinthia" ~ "kärnten",
    country_code == "AT" & match_name == "lower_austria" ~ "niederösterreich",
    country_code == "AT" & match_name == "styria" ~ "steiermark",
    country_code == "AT" & match_name == "tyrol" ~ "tirol",
    country_code == "AT" & match_name == "upper_austria" ~ "oberösterreich",
    country_code == "AT" & match_name == "vienna" ~ "wien",
    TRUE ~ match_name))


# Fixing Poland (most regions are nuts2, but "masovian_voivodeship" - "makroregion_województwo_mazowieckie" (PL9)- the region containing Warsaw - is nuts1)

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "PL" & match_name == "greater_poland_voivodeship" ~ "PL41",
    country_code == "PL" & match_name == "kuyavian-pomeranian_voivodeship" ~ "PL61",
    country_code == "PL" & match_name == "lesser_poland_voivodeship" ~ "PL21",
    country_code == "PL" & match_name == "łódź_voivodeship" ~ "PL71",
    country_code == "PL" & match_name == "lower_silesian_voivodeship" ~ "PL51",
    country_code == "PL" & match_name == "lublin_voivodeship" ~ "PL81",
    country_code == "PL" & match_name == "lubusz_voivodeship" ~ "PL43",
    country_code == "PL" & match_name == "masovian_voivodeship" ~ "PL9",
    country_code == "PL" & match_name == "opole_voivodeship" ~ "PL52",
    country_code == "PL" & match_name == "podkarpackie_voivodeship" ~ "PL82",
    country_code == "PL" & match_name == "podlaskie_voivodeship" ~ "PL84",
    country_code == "PL" & match_name == "pomeranian_voivodeship" ~ "PL63",
    country_code == "PL" & match_name == "silesian_voivodeship" ~ "PL22",
    country_code == "PL" & match_name == "swietokrzyskie" ~ "PL72",
    country_code == "PL" & match_name == "warmian-masurian_voivodeship" ~ "PL62",
    country_code == "PL" & match_name == "west_pomeranian_voivodeship" ~ "PL42",
    TRUE ~ code_2016))


# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "PL" & match_name == "greater_poland_voivodeship" ~ "wielkopolskie",
    country_code == "PL" & match_name == "kuyavian-pomeranian_voivodeship" ~ "kujawsko-pomorskie",
    country_code == "PL" & match_name == "lesser_poland_voivodeship" ~ "małopolskie",
    country_code == "PL" & match_name == "łódź_voivodeship" ~ "łódzkie",
    country_code == "PL" & match_name == "lower_silesian_voivodeship" ~ "dolnośląskie",
    country_code == "PL" & match_name == "lublin_voivodeship" ~ "lubelskie",
    country_code == "PL" & match_name == "lubusz_voivodeship" ~ "lubuskie",
    country_code == "PL" & match_name == "masovian_voivodeship" ~ "makroregion_województwo_mazowieckie",
    country_code == "PL" & match_name == "opole_voivodeship" ~ "opolskie",
    country_code == "PL" & match_name == "podkarpackie_voivodeship" ~ "podkarpackie",
    country_code == "PL" & match_name == "podlaskie_voivodeship" ~ "podlaskie",
    country_code == "PL" & match_name == "pomeranian_voivodeship" ~ "pomorskie",
    country_code == "PL" & match_name == "silesian_voivodeship" ~ "śląskie",
    country_code == "PL" & match_name == "swietokrzyskie" ~ "świętokrzyskie",
    country_code == "PL" & match_name == "warmian-masurian_voivodeship" ~ "warmińsko-mazurskie",
    country_code == "PL" & match_name == "west_pomeranian_voivodeship" ~ "zachodniopomorskie",
    TRUE ~ match_name))

#additional fixing for lodz_county
google_region_names <- google_region_names %>% mutate( code_2016 = ifelse( country_code == "PL" & grepl( "ód", match_name) & google_name != "masovian_voivodeship", "PL71", code_2016))
google_region_names <- google_region_names %>% mutate( match_name = ifelse( country_code == "PL" & grepl( "ód", match_name) & google_name != "masovian_voivodeship", "łódzkie", match_name))


# Fixing Portugal (no easy correspondence to nuts regions, no change for the moment)


# Fixing Romania

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "RO" & match_name == "argeș" ~ "RO311",
    country_code == "RO" & match_name == "bistrița-năsăud" ~ "RO112",
    country_code == "RO" & match_name == "botoșani" ~ "RO212",
    country_code == "RO" & match_name == "brașov" ~ "RO122",
    country_code == "RO" & match_name == "bucharest" ~ "RO321",
    country_code == "RO" & match_name == "călărași" ~ "RO312",
    country_code == "RO" & match_name == "caraș-severin" ~ "RO422",
    country_code == "RO" & match_name == "constanța" ~ "RO223",
    country_code == "RO" & match_name == "dâmbovița" ~ "RO313",
    country_code == "RO" & match_name == "galați" ~ "RO224",
    country_code == "RO" & match_name == "ialomița" ~ "RO315",
    country_code == "RO" & match_name == "iași" ~ "RO213",
    country_code == "RO" & match_name == "maramureș" ~ "RO114",
    country_code == "RO" & match_name == "mehedinți" ~ "RO413",
    country_code == "RO" & match_name == "mureș" ~ "RO125",
    country_code == "RO" & match_name == "neamț" ~ "RO214",
    country_code == "RO" & match_name == "timiș" ~ "RO424",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "RO" & match_name == "argeș" ~ "argeş",
    country_code == "RO" & match_name == "bistrița-năsăud" ~ "bistrița-năsăud",
    country_code == "RO" & match_name == "botoșani" ~ "botoşani",
    country_code == "RO" & match_name == "brașov" ~ "braşov",
    country_code == "RO" & match_name == "bucharest" ~ "bucureşti",
    country_code == "RO" & match_name == "călărași" ~ "călăraşi",
    country_code == "RO" & match_name == "caraș-severin" ~ "caraş-severin",
    country_code == "RO" & match_name == "constanța" ~ "constanţa",
    country_code == "RO" & match_name == "dâmbovița" ~ "dâmboviţa",
    country_code == "RO" & match_name == "galați" ~ "galaţi",
    country_code == "RO" & match_name == "ialomița" ~ "ialomiţa",
    country_code == "RO" & match_name == "iași" ~ "iaşi",
    country_code == "RO" & match_name == "maramureș" ~ "maramureş",
    country_code == "RO" & match_name == "mehedinți" ~ "mehedinţi",
    country_code == "RO" & match_name == "mureș" ~ "mureş",
    country_code == "RO" & match_name == "neamț" ~ "neamţ",
    country_code == "RO" & match_name == "timiș" ~ "timiş",
    TRUE ~ match_name))


#additional fixing for three counties where strange characters may go missing
google_region_names <- google_region_names %>% mutate( code_2016 = ifelse( country_code == "RO" & grepl( "bistr", match_name), "RO112", code_2016))
google_region_names <- google_region_names %>% mutate( match_name = ifelse( country_code == "RO" & grepl( "bistr", match_name), "bistriţa-năsăud", match_name))

google_region_names <- google_region_names %>% mutate( code_2016 = ifelse( country_code == "RO" & grepl( "cara", match_name), "RO422", code_2016))
google_region_names <- google_region_names %>% mutate( match_name = ifelse( country_code == "RO" & grepl( "cara", match_name), "caraş-severin", match_name))

google_region_names <- google_region_names %>% mutate( code_2016 = ifelse( country_code == "RO" & substr(match_name,5,6) == "ra", "RO312", code_2016))
google_region_names <- google_region_names %>% mutate( match_name = ifelse( country_code == "RO" & substr(match_name,5,6) == "ra", "călăraşi", match_name))


# Fixing Slovenia (seems like local municipal data, no change for now)


#Fixing Slovakia

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "SK" & match_name == "banská_bystrica_region" ~ "SK032",
    country_code == "SK" & match_name == "bratislava_region" ~ "SK010",
    country_code == "SK" & match_name == "košice_region" ~ "SK042",
    country_code == "SK" & match_name == "nitra_region" ~ "SK023",
    country_code == "SK" & match_name == "prešov_region" ~ "SK041",
    country_code == "SK" & match_name == "trenčín_region" ~ "SK022",
    country_code == "SK" & match_name == "trnava_region" ~ "SK021",
    country_code == "SK" & match_name == "žilina_region" ~ "SK031",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "SK" & match_name == "banská_bystrica_region" ~ "banskobystrický_kraj",
    country_code == "SK" & match_name == "bratislava_region" ~ "bratislavský_kraj",
    country_code == "SK" & match_name == "košice_region" ~ "košický_kraj",
    country_code == "SK" & match_name == "nitra_region" ~ "nitriansky_kraj",
    country_code == "SK" & match_name == "prešov_region" ~ "prešovský_kraj",
    country_code == "SK" & match_name == "trenčín_region" ~ "trenčiansky_kraj",
    country_code == "SK" & match_name == "trnava_region" ~ "trnavský_kraj",
    country_code == "SK" & match_name == "žilina_region" ~ "žilinský_kraj",
    TRUE ~ match_name))

#additional fixing for a region where strange characters may go missing
google_region_names <- google_region_names %>% mutate( code_2016 = ifelse( country_code == "SK" & grepl( "tren", match_name), "SK022", code_2016))
google_region_names <- google_region_names %>% mutate( match_name = ifelse( country_code == "SK" & grepl( "tren", match_name), "trenčiansky_kraj", match_name))



# Fixing Finland

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "FI" & match_name == "central_finland" ~ "FI193",
    country_code == "FI" & match_name == "central_ostrobothnia" ~ "FI1D5",
    country_code == "FI" & match_name == "lapland" ~ "FI1D7",
    country_code == "FI" & match_name == "north_karelia" ~ "FI1D3",
    country_code == "FI" & match_name == "northern_ostrobothnia" ~ "FI1D9",
    country_code == "FI" & match_name == "northern_savonia" ~ "FI1D2",
    country_code == "FI" & match_name == "ostrobothnia" ~ "FI195",
    country_code == "FI" & match_name == "päijänne_tavastia" ~ "FI1C3",
    country_code == "FI" & match_name == "south_karelia" ~ "FI1C5",
    country_code == "FI" & match_name == "southern_ostrobothnia" ~ "FI194",
    country_code == "FI" & match_name == "southern_savonia" ~ "FI1D1",
    country_code == "FI" & match_name == "southwest_finland" ~ "FI1C1",
    country_code == "FI" & match_name == "tavastia_proper" ~ "FI1C2",
    country_code == "FI" & match_name == "uusimaa" ~ "FI1B1",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "FI" & match_name == "central_finland" ~ "keski-suomi",
    country_code == "FI" & match_name == "central_ostrobothnia" ~ "keski-pohjanmaa",
    country_code == "FI" & match_name == "lapland" ~ "lappi",
    country_code == "FI" & match_name == "north_karelia" ~ "pohjois-karjala",
    country_code == "FI" & match_name == "northern_ostrobothnia" ~ "pohjois-pohjanmaa",
    country_code == "FI" & match_name == "northern_savonia" ~ "pohjois-savo",
    country_code == "FI" & match_name == "ostrobothnia" ~ "pohjanmaa",
    country_code == "FI" & match_name == "päijänne_tavastia" ~ "päijät-häme",
    country_code == "FI" & match_name == "south_karelia" ~ "etelä-karjala",
    country_code == "FI" & match_name == "southern_ostrobothnia" ~ "etelä-pohjanmaa",
    country_code == "FI" & match_name == "southern_savonia" ~ "etelä-savo",
    country_code == "FI" & match_name == "southwest_finland" ~ "varsinais-suomi",
    country_code == "FI" & match_name == "tavastia_proper" ~ "kanta-häme",
    country_code == "FI" & match_name == "uusimaa" ~ "helsinki-uusimaa",
    TRUE ~ match_name))


# Fixing Sweden

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "SE" & match_name == "blekinges_län" ~ "SE221",
    country_code == "SE" & match_name == "gavleborgs_län" ~ "SE313",
    country_code == "SE" & match_name == "jamtlands_län" ~ "SE322",
    country_code == "SE" & match_name == "jonkopings_län" ~ "SE211",
    country_code == "SE" & match_name == "kalmars_län" ~ "SE213",
    country_code == "SE" & match_name == "örebros_län" ~ "SE124",
    country_code == "SE" & match_name == "skånes_län" ~ "SE224",
    country_code == "SE" & match_name == "uppsalas_län" ~ "SE121",
    country_code == "SE" & match_name == "varmlands_län" ~ "SE311",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "SE" & match_name == "blekinges_län" ~ "blekinge_län",
    country_code == "SE" & match_name == "gavleborgs_län" ~ "gävleborgs_län",
    country_code == "SE" & match_name == "jamtlands_län" ~ "jämtlands_län",
    country_code == "SE" & match_name == "jonkopings_län" ~ "jönköpings_län",
    country_code == "SE" & match_name == "kalmars_län" ~ "kalmar_län",
    country_code == "SE" & match_name == "örebros_län" ~ "örebro_län",
    country_code == "SE" & match_name == "skånes_län" ~ "skåne_län",
    country_code == "SE" & match_name == "uppsalas_län" ~ "uppsala_län",
    country_code == "SE" & match_name == "varmlands_län" ~ "värmlands_län",
    TRUE ~ match_name))


# Fixing the UK. Most regions are nuts3, greater_london nuts1.
# google has many regions that are smaller than nuts3, below is the (approx) relationship
# formar: google_name(s) -> nut name(s) 
# aberdeen_city + aberdeenshire -> aberdeen_city_and_aberdeenshire
# angus_council +	dundee_city_council -> dundee_city 
# bath_and_north_east_somerset + north_somerset + south_gloucestershire -> bath_and_north_east_somerset_north_somerset_and_south_gloucestershire
# borough_of_halton part of merseyside in nuts
# bracknell_forest part of berkshire
# bridgend_county_borough + neath_port_talbot_principle_area -> bridgend_and_neath_port_talbot
# caerphilly_county_borough + blaenau_gwent -> gwent_valleys
# cardiff + vale_of_glamorgan -> cardiff_and_vale_of_glamorgan
# clackmannanshire + fife -> clackmannanshire_and_fife
# conwy_principal_area + denbighshire -> conwy_and_denbighshire
# derby + derbyshire -> 	derby
# east_ayrshire_council + north_ayrshire_council -> east_ayrshire_and_north_ayrshire_mainland
# east_dunbartonshire + west_dunbartonshire_council -> east_dunbartonshire_west_dunbartonshire_and_helensburgh_
# east_lothian_council + midlothian -> east_lothian_and_midlothian
# east_renfrewshire + inverclyde + renfrewshire -> inverclyde_east_renfrewshire_and_renfrewshire
# flintshire + wrexham_principal_area -> flintshire_and_wrexham
# hampshire + isle of Wight -> hampshire_and_isle_of_wight (nuts2)
# hartlepool + stockton-on-tees -> hartlepool_and_stockton
# leicestershire + rutland -> leicestershire_cc_and_rutland
# middlesbrough + redcar_and_cleveland -> south_teesside
# monmouthshire + newport -> monmouthshire_and_newport
# moray -> inverness_&_nairn_and_moray_badenoch_&_strathspey
# norfolk -> norwich_and_east_norfolk + north_and_west_norfolk + breckland_and_south_norfolk
# north_east_lincolnshire + north_lincolnshire -> north_and_north_east_lincolnshire
# northamptonshire -> west_northamptonshire + north_northamptonshire
# nottinghamshire -> north_nottinghamshire + south_nottinghamshire
# perth_and_kinross + stirling -> perth_&_kinross_and_stirling
# bracknell_forest + reading + slough + west_berkshire + wokingham + windsor_and_maidenhead -> berkshire
# middlesbrough + redcar_and_cleveland + stockton-on-tees + york -> north_yorkshire_cc
# surrey -> west_surrey + east_surrey
# west_sussex ->	west_sussex_(south_west) + west_sussex_(north_east)
# flintshire + wrexham_principal_area -> flintshire_and_wrexham


# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "GB" & match_name == "argyll_and_bute_council" ~ "UKM63",
    country_code == "GB" & match_name == "buckinghamshire" ~ "UKJ13",
    country_code == "GB" & match_name == "cambridgeshire" ~ "UKH12",
    country_code == "GB" & match_name == "city_of_bristol" ~ "UKK11",
    country_code == "GB" & match_name == "cornwall" ~ "UKK30",
    country_code == "GB" & match_name == "county_durham" ~ "UKC14",
    country_code == "GB" & match_name == "derry_and_strabane" ~ "UKN10",
    country_code == "GB" & match_name == "dorset" ~ "UKK22",
    country_code == "GB" & match_name == "dumfries_and_galloway" ~ "UKM92",
    country_code == "GB" & match_name == "east_sussex" ~ "UKJ22",
    country_code == "GB" & match_name == "edinburgh" ~ "UKM75",
    country_code == "GB" & match_name == "greater_london" ~ "UKI",
    country_code == "GB" & match_name == "herefordshire" ~ "UKG11",
    country_code == "GB" & match_name == "kingston_upon_hull" ~ "UKE11",
    country_code == "GB" & match_name == "na_h-eileanan_an_iar" ~ "UKM64",
    country_code == "GB" & match_name == "neath_port_talbot_principle_area" ~ "UKL17",
    country_code == "GB" & match_name == "orkney" ~ "UKM65",
    country_code == "GB" & match_name == "shropshire" ~ "UKG22",
    country_code == "GB" & match_name == "south_ayrshire_council" ~ "UKM94",
    country_code == "GB" & match_name == "staffordshire" ~ "UKG24",
    country_code == "GB" & match_name == "wiltshire" ~ "UKK15",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "GB" & match_name == "argyll_and_bute_council" ~ "lochaber_skye_&_lochalsh_arran_&_cumbrae_and_argyll_&_bute",
    country_code == "GB" & match_name == "buckinghamshire" ~ "buckinghamshire_cc",
    country_code == "GB" & match_name == "cambridgeshire" ~ "cambridgeshire_cc",
    country_code == "GB" & match_name == "city_of_bristol" ~ "bristol_city_of",
    country_code == "GB" & match_name == "cornwall" ~ "cornwall_and_isles_of_scilly",
    country_code == "GB" & match_name == "county_durham" ~ "durham_cc",
    country_code == "GB" & match_name == "derry_and_strabane" ~ "derry_city_and_strabane",
    country_code == "GB" & match_name == "dorset" ~ "dorset_cc",
    country_code == "GB" & match_name == "dumfries_and_galloway" ~ "dumfries_&_galloway",
    country_code == "GB" & match_name == "east_sussex" ~ "east_sussex_cc",
    country_code == "GB" & match_name == "edinburgh" ~ "edinburgh_city_of",
    country_code == "GB" & match_name == "greater_london" ~ "london1",
    country_code == "GB" & match_name == "herefordshire" ~ "herefordshire_county_of",
    country_code == "GB" & match_name == "kingston_upon_hull" ~ "kingston_upon_hull_city_of",
    country_code == "GB" & match_name == "na_h-eileanan_an_iar" ~ "na_h-eileanan_siar_(western_isles)",
    country_code == "GB" & match_name == "neath_port_talbot_principle_area" ~ "bridgend_and_neath_port_talbot",
    country_code == "GB" & match_name == "orkney" ~ "orkney_islands",
    country_code == "GB" & match_name == "shropshire" ~ "shropshire_cc",
    country_code == "GB" & match_name == "south_ayrshire_council" ~ "south_ayrshire",
    country_code == "GB" & match_name == "staffordshire" ~ "staffordshire_cc",
    country_code == "GB" & match_name == "wiltshire" ~ "wiltshire_cc",
    TRUE ~ match_name))


# Fixing Switzerland

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "CH" & match_name == "basel_city" ~ "CH031",
    country_code == "CH" & match_name == "canton_of_bern" ~ "CH021",
    country_code == "CH" & match_name == "canton_of_zug" ~ "CH066",
    country_code == "CH" & match_name == "fribourg" ~ "CH022",
    country_code == "CH" & match_name == "geneva" ~ "CH013",
    country_code == "CH" & match_name == "grisons" ~ "CH056",
    country_code == "CH" & match_name == "lucerne" ~ "CH061",
    country_code == "CH" & match_name == "valais" ~ "CH012",
    country_code == "CH" & match_name == "zurich" ~ "CH04",
    TRUE ~ code_2016))

# changing names
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "CH" & match_name == "basel_city" ~ "basel-stadt",
    country_code == "CH" & match_name == "canton_of_bern" ~ "bern_/_berne",
    country_code == "CH" & match_name == "canton_of_zug" ~ "zug",
    country_code == "CH" & match_name == "fribourg" ~ "fribourg_/_freiburg",
    country_code == "CH" & match_name == "geneva" ~ "genève",
    country_code == "CH" & match_name == "grisons" ~ "graubünden_/_grigioni_/_grischun",
    country_code == "CH" & match_name == "lucerne" ~ "luzern",
    country_code == "CH" & match_name == "valais" ~ "valais_/_wallis",
    country_code == "CH" & match_name == "zurich" ~ "zürich",
    TRUE ~ match_name))


#View(google_region_names %>% filter (is.na(code_2016)))

google_nuts_matchtable <- google_region_names %>%
  mutate ( typology = case_when (
    nchar(code_2016) == 5 ~ 'nuts_level_3', 
    nchar(code_2016) == 4 ~ 'nuts_level_2', 
    nchar(code_2016) == 3 ~ 'nuts_level_1', 
    nchar(code_2016) == 2 ~ 'country', 
    TRUE ~  'invalid typology'
  )) %>%
  select ( -all_of(c("google_name", "match_name")))

#create list of countries where available nuts codes do not cover full country
countries_missing_full_nuts <- google_nuts_matchtable %>%
  filter ( typology == 'invalid typology') %>% select(country_code) %>% unique() %>% unlist() %>% unname()

countries_missing_full_nuts

# Adding code_2016 values again, checking for discrepancies
google_region_names <- google_region_names %>%
  left_join ( regions_and_names_2016 %>%
                select (c(country_code, code_2016, match_name)) , 
              by = c("country_code", "match_name")) %>%
  mutate ( typology = case_when (
    nchar(code_2016) == 5 ~ 'nuts_level_3', 
    nchar(code_2016) == 4 ~ 'nuts_level_2', 
    nchar(code_2016) == 3 ~ 'nuts_level_1', 
    nchar(code_2016) == 2 ~ 'country', 
    TRUE ~  'invalid typology'
  ))


#saving results
#save(google_nuts_matchtable, file = "google_nuts_matchtable.RData")
#load("google_nuts_matchtable.RData")

usethis::use_data(google_nuts_matchtable, 
                  internal=FALSE, overwrite = TRUE)
data ( google_nuts_matchtable )
