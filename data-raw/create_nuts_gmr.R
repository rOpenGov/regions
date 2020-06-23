gmr <- gmr_csv %>%
  set_names ( c("country_code", "google_country_name", 
                "google_region_name_1", 
                "google_region_name_2", 
                "date", "retail", "grocery", 
                "parks", "transit", "workplaces", "residential") 
              ) %>%
  mutate ( google_region_name_1 = ifelse (
    #La Réunion is part of France and data can be found about it
    #in Eurostat and French national statistics
               country_code == "RE", 
               yes = "La Réunion", no = google_region_name_1), 
           google_country_name = ifelse ( 
               country_code == "RE", 
               yes = "France",no = google_country_name ), 
   country_code = ifelse ( 
     ## La Réunion is also known as Réunion and it has a 
     ## ISO-3166-1 code even though it is part of France
               country_code == "RE", 
               yes = "FR", no = country_code )
   )

## First joining with valid NUTS codes ------------
## When there is no region name, use the country name
## And preferably the English country name, not the national language one


data("all_valid_nuts_codes", package = 'regions')
all_valid_nuts_codes 


nuts_gmr <- all_valid_nuts_codes %>%
  mutate ( country_code = get_country_code(geo) ) %>%
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
  filter ( !is.na(google_name) )

names ( nuts_lau_2019 )

nuts_lau_2019 %>%
  select ( code_2016, lau_code, population ) %>%
  unite ( code_2016, c("code_2016", "lau_code"), sep = '_')

## latvia and slovenia are organized by municipality
european_gmr_by_municipality  <- nuts_gmr %>%
  left_join ( google_nuts_matchtable, 
              by = c("country_code", 
                     "google_region_level", "google_region_name")) %>%
  filter ( typology == 'nuts_level_3_lau') %>%
  left_join ( nuts_lau_2019 %>%
                select ( code_2016, lau_code, population ) %>%
                unite ( code_2016, c("code_2016", "lau_code"),
                        sep = '_'), 
              by = 'code_2016') %>%
  select ( all_of (c("country_code", "date", 
                     "code_2016", "population", 
                     "retail", "grocery", "parks", 
                     "transit", "workplaces", "residential"))) %>%
  group_by ( code_2016 ) %>%
  arrange  ( code_2016, date ) %>%
  fill (  # downup uses first the upper (earlier) date, then 
          # the lower (later) date to fill missing blanks observations
          # by date.
          retail, grocery, parks, transit, workplaces, residential, 
         .direction = "downup")

google_nuts_matchtable %>%
  filter ( country_code == "EE")

european_gmr_by_nuts3_ext  <- nuts_gmr %>%
  left_join ( google_nuts_matchtable, 
              by = c("country_code", 
                     "google_region_level", "google_region_name")) %>%
  filter ( country_code %in% c("EE", "PT")) 
