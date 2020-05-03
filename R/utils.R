#' Validate Parameter 'dat'

validate_data_frame <- function(dat) {
  
  if ( ! "data.frame" %in% class(dat) ) {
    stop ("The parameter 'dat' must be a data.frame-like object, like a data frame or a tibble.")
  } 
  
  if ( nrow(dat) == 0 ) {
    stop ( "The data frame has no observations.")
  }
  
}
