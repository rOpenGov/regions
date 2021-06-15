#' R&D Personnel by NUTS 2 Regions 
#'
#' A subset of the Eurostat dataset 
#' \code{R&D personnel and researchers by sector of performance, sex and NUTS 2 regions}.
#' 
#' Mapping Regional Data, Mapping Metadata Problem
#'
#' The fresh version of this statistic can be obtained by
#' \code{eurostat::get_eurostat_json (id = "rd_p_persreg", 
#' filters = list (sex = "T", prof_pos = "TOTAL",sectperf = "TOTAL", unit = "FTE" ))
#' }
#' 
#' @format A data frame with 956 observations of 7 variables:
#' \describe{
#'   \item{geo}{National and sub-national geographical codes from Eurostat}
#'   \item{time}{Time, coded as a numeric variable of the year, 2006-2019}
#'   \item{values}{The numeric statistical values}
#'   \item{unit}{Unit of measurement, contains only FTE}
#'   \item{sex}{Sex of researchers, contains only both sexes as T}
#'   \item{prof_pos}{Professional position, contains all R&D employees not only researchers}
#'   \item{sectperf}{Sector of performance, filtered for all sectors as TOTAL}
#'   
#' }
#' @source \url{https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=rd_p_persreg&lang=en}
#' @seealso recode_nuts
"regional_rd_personnel"
