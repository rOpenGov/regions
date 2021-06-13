
library(eurostat)
rd_workforce <- eurostat::get_eurostat_json (
  id = "rd_p_persreg", 
  filters = list (
    sex = "T",     
    prof_pos = "TOTAL",
    sectperf = "TOTAL", 
    unit = "FTE" )
)

regional_rd_personnel <- rd_workforce  %>%
  dplyr::filter ( .data$time %in% c("2009", "2018") )

if ( nrow(regional_rd_personnel) > 0) {
  usethis::use_data ( regional_rd_personnel, overwrite = TRUE) 
}

regional_rd_workforce

