## You can use the internal function create_nuts_lau_2019 as a template
## for historical LAU correspondence tables, if needed.

nuts_lau_2019 <- regions:::create_nuts_lau_2019()

usethis::use_data(nuts_lau_2019, overwrite = TRUE, internal = FALSE)

