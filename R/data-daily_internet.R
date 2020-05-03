#' Daily Internet Users
#'
#' A dataset containing the percentage of individuals who used the Internet on
#' a daily basis in the European countries and regions. 
#' 
#' The fresh version of this statistic can be obtained by 
#' \code{eurostat::get_eurostat("isoc_r_iuse_i", time_format = "num")}
#' and filtered for the \code{indic_is = "I_IDAY"} indicator and the 
#' \code{unit="PC_IND"} unit.
#' 
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{geo}{National and sub-national geographical codes from Eurostat}
#'   \item{time}{Time, coded as a numeric variable of the year, 2006-2019}
#'   \item{values}{The numeric statistical values}
#' }
"daily_internet_users"
