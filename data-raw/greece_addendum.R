## Fixing Greece

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
    TRUE ~ code_2016))
