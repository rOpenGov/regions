
#	lincolnshire is duplicated, both nuts2 and 3
# code at the end filters out nuts2 region


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
google_region_names <- google_region_names %>% filter (code_2016 != "UKF3")


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

