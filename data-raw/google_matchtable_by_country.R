## Google region names before national corrections ------------
google_region_names <- nuts_gmr %>%
  select ( country_code, google_region_level,
           google_region_name, google_name ) %>%
  filter ( complete.cases(.)) %>%
  distinct_all() %>% 
  mutate ( match_name = google_name )

#some general changes for better fit
#switching for local county name in SE
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse(
    (country_code == "SE" & grepl( "_county", match_name)),
    gsub("_county", "s_län", match_name), match_name)
  ) 

#getting rid of "_county" from names in RO
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse(
    (country_code == "RO" & 
       grepl( "_county", match_name)), 
    gsub("_county", "", match_name), match_name)) 

# Adding code_2016 values where match is possible
google_region_names <- google_region_names %>%
  left_join ( regions_and_names_2016 %>% 
                select (c(country_code, code_2016, match_name)) , 
              by = c("country_code", "match_name"))

## Fixing Hungary ------------------------------------------------
google_region_names <- google_region_names %>%
  mutate ( match_name = case_when (
    country_code == "HU" & grepl("moson-sopron", match_name) ~  hungary_names[grepl("moson-sopron", hungary_names)],
    country_code == "HU" & grepl( "hungary|budapest", match_name) ~ match_name,
    country_code == "HU" & grepl( "_county", match_name) ~ gsub("_county", "", match_name), 
    TRUE ~ match_name))

# Fixing Italy ------------------------------------------------------
# See The Typology Of The Google Mobility Reports (COVID-19) vignette
# for Trentino-South Tyrol

# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "IT" & match_name == "aosta" ~ "ITCX",
    country_code == "IT" & match_name == "aosta" ~ "ITC2",
    country_code == "IT" & match_name == "apulia" ~ "ITF4",
    country_code == "IT" & match_name == "lombardy" ~ "ITC4",
    country_code == "IT" & match_name == "piedmont" ~ "ITC1",
    country_code == "IT" & match_name == "sardinia" ~ "ITG2",
    country_code == "IT" & match_name == "sicily" ~ "ITG1",
    country_code == "IT" & match_name == "tuscany" ~ "ITI1",
    country_code == "IT" & match_name == "trentino-south_tyrol" ~ "ITDX", #this is a pseudo-code, because these are two regions
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

## Fixing Belgium -----------------------------------------------
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


## Fixing Czechia --------------------------------------
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
google_region_names <- google_region_names %>%
  mutate( code_2016 = ifelse( country_code == "CZ" & grepl( "plze", match_name),
                              "CZ032", code_2016))
google_region_names <- google_region_names %>%
  mutate( match_name = ifelse( country_code == "CZ" & grepl( "plze", match_name), 
                               "plzeňský_kraj", match_name))

# Fixing Denmark -----------------------------------------------------
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

# Fixing Germany ----------------------------------------------
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

# Fixing Estonia ------------------------------------------
# 
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse(
    ( test = country_code == "EE" &  grepl( "_county", match_name)), 
    yes  = gsub("_county", "", match_name), 
    no   =  match_name)) %>%
  mutate ( code_2016 = case_when (
    country_code == "EE" & match_name == "harju" ~ "EE001",
    country_code == "EE" & match_name == "ida-viru" ~ "EE007",
    country_code == "EE" & match_name == "hiiu" ~ "EE004H",
    country_code == "EE" & match_name == "saare" ~ "EE004S",
    country_code == "EE" & match_name == "pärnu" ~ "EE004P",
    country_code == "EE" & match_name == "lääne" ~ "EE004L",
    country_code == "EE" & match_name == "järva" ~ "EE006J",
    country_code == "EE" & match_name == "lääne-viru" ~ "EE006L",
    country_code == "EE" & match_name == "rapla" ~ "EE006R",
    country_code == "EE" & match_name == "jõgeva" ~ "EE008J",
    country_code == "EE" & match_name == "põlva" ~ "EE008P",
    country_code == "EE" & match_name == "tartu" ~ "EE008T",
    country_code == "EE" & match_name == "valga" ~ "EE008V",
    country_code == "EE" & match_name == "viljandi" ~ "EE008I",
    country_code == "EE" & match_name == "võru" ~ "EE008U", 
    TRUE ~ code_2016)) %>%
  mutate ( match_name = case_when (
    country_code == "EE" & match_name == "harju" ~ paste0("northern estonia ", match_name),
    country_code == "EE" & match_name == "ida-viru" ~ paste0("northeastern estonia ", match_name), 
    country_code == "EE" & match_name %in% c("hiiu","saare" ,"pärnu", "lääne") ~ paste0("western estonia ", match_name),
    country_code == "EE" & match_name %in% c("järva","lääne-viru" ,"rapla") ~ paste0("central estonia", match_name),
    country_code == "EE" & match_name %in% c("jõgeva", "põlva", "tartu", "valga", 
                                             "viljandi","võru" ) ~ paste0("southern estonia ", match_name),
    TRUE ~ match_name))

#EE004 Western Estonia (Hiiu, Lääne, Pärnu and Saare county)
#EE006 Central Estonia (Järve, Lääne-Viru and Rapla county)
#EE007 Northeastern Estonia (Ida-Viru county)
#EE008 Southern Estonia (Jõgeva, Põlva, Tartu, Valga, Viljandi and Võru county)
# https://www.stat.ee/296050#:~:text=EE004%20Western%20Estonia%20(Hiiu%2C%20L%C3%A4%C3%A4ne,Valga%2C%20Viljandi%20and%20V%C3%B5ru%20county)

google_region_names_ee <- google_region_names  %>%
  filter ( country_code == "EE")

# Fixing Ireland ------------------------------------
# (Local government level data only, only Dublin County corresponds to NUTS3, skipping the rest for the moment)
# changing nuts code
google_region_names <- google_region_names %>%
  mutate ( code_2016 = ifelse(
    country_code == "IE" & match_name == "county_dublin", 
    "IE061", code_2016)) 

# changing name
google_region_names <- google_region_names %>%
  mutate ( match_name = ifelse(
    country_code == "IE" & match_name == "county_dublin",
    "dublin", match_name)) 

# Fixing Spain -----------------------------------------
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


#Fixing France --------------------------------------------
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


# Fixing Croatia ------------------------------------------
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


# Fixing Latvia (Municipal data only partially finished) ----------
# PARTIAL

google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "LV" & match_name == "riga" ~ "LV006",
    country_code == "LV" & match_name == "city_of_liepāja" ~ "LV003L",
    country_code == "LV" & match_name == "ventspils" ~ "LV003V",
    country_code == "LV" & match_name == "saldus_municipality" ~ "LV003S",
    country_code == "LV" & match_name == "talsi_municipality" ~ "LV003T",
    country_code == "LV" & match_name == "dobele_municipality" ~ "LV003D",
    country_code == "LV" & match_name == "ādaži_municipality" ~ "LV007A",
    country_code == "LV" & match_name == "aizkraukle_municipality" ~ "LV009A",
    country_code == "LV" & match_name == "aloja_municipality" ~ "LV007L",
    country_code == "LV" & match_name == "alūksne_municipality" ~ "LV008A",
    country_code == "LV" & match_name == "babīte_municipality" ~ "LV007B",
    country_code == "LV" & match_name == "baldone_municipality" ~ "LV007X",
    country_code == "LV" & match_name == "balvi_municipality" ~ "LV005B",
    country_code == "LV" & match_name == "bauska_municipality" ~ "LV009B",
    country_code == "LV" & match_name == "burtnieki_municipality" ~ "LV008B",
    country_code == "LV" & match_name == "carnikava_municipality" ~ "LV007C",
    country_code == "LV" & match_name == "cēsis_municipality" ~ "LV008C",
    country_code == "LV" & match_name == "daugavpils" ~ "LV005D",
    country_code == "LV" & match_name == "daugavpils_municipality" ~ "LV005M",
    country_code == "LV" & match_name == "engure_municipality" ~ "LV007E",
    country_code == "LV" & match_name == "iecava_municipality" ~ "LV009I",
    country_code == "LV" & match_name == "jaunjelgava_municipality" ~ "LV009J",
    country_code == "LV" & match_name == "latvia" ~ "LV00",
    country_code == "LV"                 ~ "LV00X",
    TRUE ~ code_2016)
  )

google_lv <- google_region_names %>%
  filter ( country_code == "LV")

# Fixing Lithuania ---------------------------
# (chaning "_county" to "_apskritis" in name, this should match nuts3 regions)

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


# Fixing 	Luxembourg ------------------------------------------
# bringig to lowest identifiable level, which is LU000
# Country has no NUTS subdivisions
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "LU"  ~ "LU000",
    TRUE ~ code_2016)
  )

# Fixing Hungary (names were changed at the beginning)


# Fixing 	Malta ------------------------
# Country has two NUTS3 subdivisions, but Google do not make
# subdivisions. Each brought to MT00 = MT0 = MT

google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "MT"  ~ "MT00",
    TRUE ~ code_2016)
  )

# Fixing the Netherlands ----------------------------------------
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


# Fixing Austria ---------------------------------------
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
google_region_names <- google_region_names %>%
  mutate( code_2016 = ifelse( country_code == "PL" & grepl( "ód", match_name) & google_name != "masovian_voivodeship", "PL71", code_2016))
google_region_names <- google_region_names %>%
  mutate( match_name = ifelse( country_code == "PL" & grepl( "ód", match_name) & google_name != "masovian_voivodeship", "łódzkie", match_name))


# Fixing Portugal -------------------------------------------------
# Only approximate solution, Google uses ISO-3166-2 which is not 
# compatible with NUTS
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "PT" & match_name == "lisbon"   ~ "PT17",
    country_code == "PT" & match_name == "azores"   ~ "PT20",
    country_code == "PT" & match_name == "madeira"  ~ "PT30",
    country_code == "PT" & match_name == "aveiro_district" ~ "PT16D",
    country_code == "PT" & match_name == "faro_district" ~ "PT16D",
    country_code == "PT" & match_name == "setubal" ~ "PT181",
    country_code == "PT" & match_name == "beja_district" ~ "PT184", #with one village in PT181
    country_code == "PT" & match_name == "santarém_district" ~ "PT16Y", #part of Centro, historical region with cross-boundary changes
    country_code == "PT" & match_name == "porto_district" ~ "PT11A",
    country_code == "PT" & match_name == "évora_district" ~ "PT187",
    country_code == "PT" & match_name == "portalegre_district" ~ "PT187",
    country_code == "PT" & match_name == "guarda_district" ~ "PT16I", #except for one municipality in Norte
    country_code == "PT" & match_name == "braga" ~ "PT11X", #Cross-boundary changes, part of Norte, PT112
    country_code == "PT" & match_name == "bragança_district" ~ "PT11E", #Cross-boundary changes, not fully the same
    country_code == "PT" & match_name == "castelo_branco_district" ~ "PT16X", #Cross-boundary changes, part of Centro
    country_code == "PT" & match_name == "vila_real_district" ~ "PT11W", #Cross-boundary changes, part of Norte
    country_code == "PT" & match_name == "viana_do_castelo_district" ~ "PT111",
    country_code == "PT" & match_name == "leiria_district" ~ "PT16F",
    country_code == "PT" & match_name == "coimbra_district" ~ "PT16E",
    country_code == "PT" & match_name == "viseu_district"~ "PT16W", #Cross-boundary changes, part of Centro
    TRUE ~ code_2016)
  ) %>%
  arrange ( code_2016 )




# Fixing Slovenia ---------------------------------------------
#(seems like local municipal data, no change for now)

slovenia_municipalities <- readr::read_csv(
  file.path('data-raw', 'slovenia_municipalities_nuts.csv'),
                           trim_ws = TRUE) 

slovenia_help_table <- google_region_names %>%
  filter ( country_code == "SI") %>%
  left_join ( 
    slovenia_municipalities %>%
      rename ( google_region_name = municipality ), 
    by = "google_region_name"
    ) %>%
  mutate ( code_2016_3 = case_when (
    country_code == "SI" & match_name == "administrative_unit_maribor"   ~ "SI032",
    country_code == "SI" & match_name == "municipality_of_hrastnik" ~ "SI035",
    country_code == "SI" & match_name == "slovenia"   ~ "SI",
    TRUE ~ code_2016_3
  )) %>%
  mutate ( code_2016 = ifelse (!is.na(code_2016_3), 
                               code_2016_3, 
                               code_2016)) %>%
  select ( all_of(c("match_name", "code_2016"))) %>%
  mutate ( code_2016_4 = paste0(code_2016,
                              toupper(substr(match_name,1,1)))) %>%
  mutate ( code_2016_4 = case_when ( 
    match_name == "logatec" ~ paste0(code_2016, "X"), 
    match_name == "ptuj" ~ paste0(code_2016, "J"),
    match_name == "šmarje_pri_jelšah" ~ paste0(code_2016, "J"),
    match_name == "ravne_na_koroškem" ~ paste0(code_2016, "K"),
    match_name == "lenart" ~ paste0(code_2016, "X"),
    match_name == "ljutomer" ~ paste0(code_2016, "Y"),
    match_name == "slovenia" ~ "SI", 
    TRUE ~ code_2016_4)) %>%
  add_count ( code_2016, code_2016_4 ) %>%
  ungroup() %>%
  select ( all_of(c("match_name", "code_2016_4")) )

google_region_names <- google_region_names %>%
  left_join (slovenia_help_table, by = "match_name" ) %>%
  mutate ( code_2016 = ifelse (test = is.na(code_2016), 
                               yes  = code_2016_4, 
                               no   = code_2016)) %>%
  select ( -code_2016_4 )
  
  #mutate ( code_2016 = case_when (
  #   country_code == "SK" & match_name == "žilina_region" ~ "SK031",
  #  TRUE ~ code_2016))

# Fixing Slovakia-------------------------------------------------
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

# Fixing Finland -------------------------------------------------

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


# Fixing Sweden ---------------------------------------------
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

# Fixing Switzerland ------------------------------------------

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


# Fixing UK ------------------------------------------------------

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
    country_code == "GB" & match_name == "orkney" ~ "UKM65",
    country_code == "GB" & match_name == "shropshire" ~ "UKG22",
    country_code == "GB" & match_name == "south_ayrshire_council" ~ "UKM94",
    country_code == "GB" & match_name == "staffordshire" ~ "UKG24",
    country_code == "GB" & match_name == "wiltshire" ~ "UKK15",
    
    country_code == "GB" & match_name == "aberdeen_city" ~ "UKM50", # this is where we have more than one name that makes up a region
    country_code == "GB" & match_name == "aberdeenshire" ~ "UKM50",
    country_code == "GB" & match_name == "angus_council" ~ "UKM21",
    country_code == "GB" & match_name == "dundee_city_council" ~ "UKM21",
    country_code == "GB" & match_name == "bath_and_north_east_somerset" ~ "UKK12",
    country_code == "GB" & match_name == "north_somerset" ~ "UKK12",
    country_code == "GB" & match_name == "south_gloucestershire" ~ "UKK12",
    country_code == "GB" & match_name == "blaenau_gwent" ~ "UKL16",
    country_code == "GB" & match_name == "caerphilly_county_borough" ~ "UKL16",
    country_code == "GB" & match_name == "torfaen_principal_areaa" ~ "UKL16",
    country_code == "GB" & match_name == "borough_of_halton" ~ "UKD71", #Knowsley and St. Helens missing, not a full nuts3 region
    country_code == "GB" & match_name == "bracknell_forest" ~ "UKJ11", #part of Berkshire nuts3 region (UKJ11)
    country_code == "GB" & match_name == "reading" ~ "UKJ11",
    country_code == "GB" & match_name == "wokingham" ~ "UKJ11",
    country_code == "GB" & match_name == "west_berkshire" ~ "UKJ11",
    country_code == "GB" & match_name == "bracknell_forest" ~ "UKJ11",
    country_code == "GB" & match_name == "slough" ~ "UKJ11",
    country_code == "GB" & match_name == "windsor_and_maidenhead" ~ "UKJ11",
    country_code == "GB" & match_name == "bridgend" ~ "UKL17",
    country_code == "GB" & match_name == "neath_port_talbot_principle_area" ~ "UKL17",
    country_code == "GB" & match_name == "cardiff" ~ "UKL22",
    country_code == "GB" & match_name == "vale_of_glamorgan" ~ "UKL22",
    country_code == "GB" & match_name == "ceredigion" ~ "UKL14",
    country_code == "GB" & match_name == "carmarthenshire" ~ "UKL14",
    country_code == "GB" & match_name == "pembrokeshire" ~ "UKL14",
    country_code == "GB" & match_name == "clackmannanshire" ~ "UKM72",
    country_code == "GB" & match_name == "fife" ~ "UKM72",
    country_code == "GB" & match_name == "conwy_principal_area" ~ "UKL13",
    country_code == "GB" & match_name == "denbighshire" ~ "UKL13",
    country_code == "GB" & match_name == "derby" ~ "UKF1", # only nuts2 gives full, consistent coverage
    country_code == "GB" & match_name == "derbyshire" ~ "UKF1",
    country_code == "GB" & match_name == "nottingham" ~ "UKF1",
    country_code == "GB" & match_name == "nottinghamshire" ~ "UKF1",
    country_code == "GB" & match_name == "east_ayrshire_council" ~ "UKM93",
    country_code == "GB" & match_name == "north_ayrshire_council" ~ "UKM93",
    country_code == "GB" & match_name == "east_dunbartonshire_council	" ~ "UKM81", #helensburgh_&_lomond missing, may not be a full nuts3 region
    country_code == "GB" & match_name == "west_dunbartonshire_council" ~ "UKM81", #helensburgh_&_lomond missing, may not be a full nuts3 region
    country_code == "GB" & match_name == "inverclyde" ~ "UKM83",
    country_code == "GB" & match_name == "east_renfrewshire_council" ~ "UKM83",
    country_code == "GB" & match_name == "renfrewshire" ~ "UKM83",
    country_code == "GB" & match_name == "flintshire" ~ "UKL23",
    country_code == "GB" & match_name == "wrexham_principal_area" ~ "UKL23",
    country_code == "GB" & match_name == "hampshire" ~ "UKJ3", # only nuts2 gives full, consistent coverage
    country_code == "GB" & match_name == "isle_of_wight" ~ "UKJ3",
    country_code == "GB" & match_name == "portsmouth" ~ "UKJ3",
    country_code == "GB" & match_name == "southampton" ~ "UKJ3",
    country_code == "GB" & match_name == "hartlepool" ~ "UKC11",
    country_code == "GB" & match_name == "stockton-on-tees" ~ "UKC11",
    country_code == "GB" & match_name == "highland_council" ~ "UKM6", #NOT EXACT MATCH!!! nuts2
    country_code == "GB" & match_name == "merthyr_tydfil_county_borough" ~ "UKL15",
    country_code == "GB" & match_name == "rhondda_cynon_taff	" ~ "UKL15",
    country_code == "GB" & match_name == "middlesbrough" ~ "UKC12",
    country_code == "GB" & match_name == "redcar_and_cleveland" ~ "UKC12",
    country_code == "GB" & match_name == "midlothian" ~ "UKM73",
    country_code == "GB" & match_name == "east_lothian_council" ~ "UKM73",
    country_code == "GB" & match_name == "monmouthshire" ~ "UKL21",
    country_code == "GB" & match_name == "newport" ~ "UKL21",
    country_code == "GB" & match_name == "moray" ~ "UKM62", # may not be full nuts3 region, rest of inverness_&_nairn_and_moray_badenoch_&_strathspey missing
    country_code == "GB" & match_name == "norfolk" ~ "UKH1",# only nuts2 gives full, consistent coverage
    country_code == "GB" & match_name == "cambridgeshire_cc" ~ "UKH1",
    country_code == "GB" & match_name == "peterborough" ~ "UKH1",
    country_code == "GB" & match_name == "suffolk" ~ "UKH1",
    country_code == "GB" & match_name == "north_east_lincolnshire" ~ "UKE13",
    country_code == "GB" & match_name == "north_lincolnshire" ~ "UKE13",
    country_code == "GB" & match_name == "northamptonshire" ~ "UKF2", # only nuts2 gives full, consistent coverage
    country_code == "GB" & match_name == "leicester" ~ "UKF2",
    country_code == "GB" & match_name == "leicestershire" ~ "UKF2",
    country_code == "GB" & match_name == "rutland" ~ "UKF2",
    country_code == "GB" & match_name == "perth_and_kinross" ~ "UKM77",
    country_code == "GB" & match_name == "stirling" ~ "UKM77",
    country_code == "GB" & match_name == "surrey" ~ "UKJ2", # only nuts2 gives full, consistent coverage
    country_code == "GB" & match_name == "brighton_and_hove" ~ "UKJ2",
    country_code == "GB" & match_name == "east_sussex_cc" ~ "UKJ2",
    country_code == "GB" & match_name == "west_sussex" ~ "UKJ2",
    country_code == "GB" & match_name == "tyne_and_wear" ~ "UKC2", # only nuts2 gives consistent coverage, Sunderland (UKC23) missing 
    country_code == "GB" & match_name == "northumberland" ~ "UKC2",
    country_code == "GB" & match_name == "north_yorkshire" ~ "UKE22", # this is the name of a nuts2 region, but it seem ro refer to nuts3 one (north_yorkshire_cc)
    TRUE ~ code_2016))

# filter out lincolnshire nuts2 (exist in nuts3 too)
google_region_names <- google_region_names %>% filter (code_2016 != "UKF3" | is.na(code_2016))

google_region_names_it <- google_region_names %>% 
  filter (country_code == "IT")

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
    country_code == "GB" & match_name == "greater_london" ~ "london",
    country_code == "GB" & match_name == "herefordshire" ~ "herefordshire_county_of",
    country_code == "GB" & match_name == "kingston_upon_hull" ~ "kingston_upon_hull_city_of",
    country_code == "GB" & match_name == "na_h-eileanan_an_iar" ~ "na_h-eileanan_siar_(western_isles)",
    country_code == "GB" & match_name == "orkney" ~ "orkney_islands",
    country_code == "GB" & match_name == "shropshire" ~ "shropshire_cc",
    country_code == "GB" & match_name == "south_ayrshire_council" ~ "south_ayrshire",
    country_code == "GB" & match_name == "staffordshire" ~ "staffordshire_cc",
    country_code == "GB" & match_name == "wiltshire" ~ "wiltshire_cc",
    TRUE ~ match_name))


## Fixing Greece --------------------------------------------
# changing nuts codes
google_region_names <- google_region_names %>%
  mutate ( code_2016 = case_when (
    country_code == "GR" & match_name == "crete_region" ~ "EL4",
    country_code == "GR" & match_name == "decentralized_administration_of_attica" ~ "EL3",
    country_code == "GR" & match_name == "decentralized_administration_of_epirus_and_western_macedonia" ~ "EL5",
    country_code == "GR" & match_name == "decentralized_administration_of_macedonia_and_thrace" ~ "EL5",
    country_code == "GR" & match_name == "decentralized_administration_of_peloponnese_western_greece_and_the_ionian" ~ "EL6",
    country_code == "GR" & match_name == "decentralized_administration_of_the_aegean" ~ "EL4",
    country_code == "GR" & match_name == "decentralized_administration_of_thessaly_and_central_greece" ~ "EL6",
    TRUE ~ code_2016)
  )

