library(regions)
library(tidyverse)
#' @author István Zsoldos, Daniel Antal
#' Not included in package, only as helper function to create the data file.

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
                               'Global_Mobility_Report_20200603.csv'))

gmr <- gmr_csv %>%
  set_names ( c("country_code", "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2", 
                "date", "retail", "grocery", 
                "parks", "transit", "workplaces", "residential") ) %>%
  mutate ( google_region_name_1 = ifelse ( country_code == "RE", 
                                           "La Réunion", 
                                           google_region_name_1), 
           google_country_name = ifelse ( country_code == "RE", 
                                          "France", 
                                          google_country_name ), 
           country_code = ifelse ( country_code == "RE", 
                                   "FR", 
                                   country_code ))

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

## Making google_region_names$match_name equal to regions_and_names_2016$match_name when there is a 1-to-1 correspondence

### Most of the country fixes are in a separate file now, 
### for easier readability.  Only BG and RO stayed here because of
### character coding issues.

source(file.path('data-raw', 'google_matchtable_by_country.R'))

##  What was found at first try  ---------------------------------

found_in_nuts_distinct <- google_region_names %>%
  left_join ( regions_and_names_2016 , 
              by = c("country_code", "match_name"))


### The following codes, due to character coding problems on Windows
### do not read well if you use source.  They have to run from 
### the code with "Run"

## Fixing Bulgaria -----------------------------------------------
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

# Fixing Romania -----------------------------------
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
google_region_names <- google_region_names %>%
  mutate( code_2016 = ifelse( country_code == "RO" & grepl( "bistr", match_name), "RO112", code_2016))
google_region_names <- google_region_names %>%
  mutate( match_name = ifelse( country_code == "RO" & grepl( "bistr", match_name), "bistriţa-năsăud", match_name))

google_region_names <- google_region_names %>%
  mutate( code_2016 = ifelse( country_code == "RO" & grepl( "cara", match_name), "RO422", code_2016))
google_region_names <- google_region_names %>%
  mutate( match_name = ifelse( country_code == "RO" & grepl( "cara", match_name), "caraş-severin", match_name))

google_region_names <- google_region_names %>%
  mutate( code_2016 = ifelse( country_code == "RO" & substr(match_name,5,6) == "ra", "RO312", code_2016))
google_region_names <- google_region_names %>%
  mutate( match_name = ifelse( country_code == "RO" & substr(match_name,5,6) == "ra", "călăraşi", match_name))

# creating the matchtable ----------------------------------
names ( google_region_names)

source( file.path('data-raw', 'google_matchtable_by_municipality.R'))

google_region_names <- google_region_names %>%
  filter ( ! country_code %in% c("LV", "SI"))  %>%
  dplyr::bind_rows ( 
    ## Add LV & SI with pseudo-NUTS codes, containing NUTS3 info combined
    ## with LAU_code of the municipality
    ## This approach will not work with Portugal.
    ## LAU data is not available for Norway
    ## Google data is not available for IS, CY
    google_lau_si_lv )

google_nuts_matchtable <- google_region_names %>%
  validate_nuts_regions(., geo_var = 'code_2016') %>%
  select ( -all_of(c("google_name", "match_name")))  %>%
  mutate ( typology = case_when (
    nchar(code_2016) >5 & country_code %in% c("SI", "LV") ~ 'nuts_level_3_lau',
    nchar(code_2016) >5 & country_code == "EE"~ 'nuts_level_3_iso-3166-2',
    nchar(code_2016) == 6 ~ 'nuts_level_3_ext',
    TRUE ~  'invalid typology'
  ))

test <- google_nuts_matchtable %>%
  filter ( country_code %in% c("LV", "EE", "PT", "SI", 'HU'))

#create list of countries where available nuts codes do not cover full country
countries_missing_full_nuts <- google_nuts_matchtable %>%
  filter ( typology == 'invalid typology') %>%
  select(country_code) %>%
  unique() %>% unlist() %>% unname()

countries_missing_full_nuts

# Adding code_2016 values again, checking for discrepancies
google_region_names_testing <- google_region_names %>%
  left_join ( regions_and_names_2016 %>%
                select (c(country_code, code_2016, match_name)), 
              by = c("country_code", "match_name", "code_2016")) %>%
  mutate ( typology = case_when (
    nchar(code_2016) == 5 ~ 'nuts_level_3', 
    nchar(code_2016) == 4 ~ 'nuts_level_2', 
    nchar(code_2016) == 3 ~ 'nuts_level_1', 
    nchar(code_2016) == 2 ~ 'country', 
    nchar(code_2016) >= 9 ~ 'nuts_level_3_lau',
    nchar(code_2016) == 6 ~ 'nuts_level_3_ext',
    TRUE ~  'invalid typology'
  ))


#saving results
#save(google_nuts_matchtable, file = "google_nuts_matchtable.RData")
#load("google_nuts_matchtable.RData")

usethis::use_data(google_nuts_matchtable, 
                  internal=FALSE,
                  overwrite = TRUE)
data ( google_nuts_matchtable )
str ( google_nuts_matchtable )
