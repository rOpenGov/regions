australia_states <- read.csv(file.path('data-raw',  'australia_states.csv'),
                             stringsAsFactors = FALSE)

australia_states$country_code <- "AU"

usethis::use_data(australia_states, overwrite = TRUE, internal = FALSE)


