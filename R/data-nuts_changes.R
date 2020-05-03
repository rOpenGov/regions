#' European Union: Recoded NUTS units 1995-2021. 
#'
#' A dataset containing the joined correspondence tables of the 
#' EU NUTS typologies.
#'
#' @format A data frame with 3097 rows and 22 variables:
#' \describe{
#'   \item{typology}{country, NUTS1, NUTS2 or NUTS3}
#'   \item{start_year}{The year when the code was first used}
#'   \item{end_year}{The year when the code was last used}
#'   \item{code_1999}{NUTS code in the 2003 definition}
#'   \item{code_2003}{NUTS code in the 2003 definition}
#'   \item{code_2006}{NUTS code in the 2006 definition}
#'   \item{code_2010}{NUTS code in the 2010 definition}
#'   \item{code_2013}{NUTS code in the 2013 definition}
#'   \item{code_2016}{NUTS code in the 2016 definition}
#'   \item{code_2021}{NUTS code in the 2021 definition}
#'   \item{geo_name_2003}{NUTS territorial name in the 2003 definition}
#'   \item{geo_name_2006}{NUTS territorial name in the 2006 definition}
#'   \item{geo_name_2010}{NUTS territorial name in the 2010 definition}
#'   \item{geo_name_2013}{NUTS territorial name in the 2013 definition}
#'   \item{geo_name_2016}{NUTS territorial name in the 2016 definition}
#'   \item{geo_name_2021}{NUTS territorial name in the 2021 definition}
#'   \item{change_2003}{Change described in the 2003 correspondence table}
#'   \item{change_2006}{Change described in the 2006 correspondence table}
#'   \item{change_2010}{Change described in the 2010 correspondence table}
#'   \item{change_2013}{Change described in the 2013 correspondence table}
#'   \item{change_2016}{Change described in the 2016 correspondence table}
#'   \item{change_2021}{Change described in the 2021 correspondence table}
#' }
#' @source \url{https://ec.europa.eu/eurostat/web/nuts/history/}
"nuts_changes"
