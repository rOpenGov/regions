
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regions

<!-- badges: start -->

“[![Last-changedate](https://img.shields.io/badge/last%20change-2020--05--02-yellowgreen.svg)](/commits/master)”
[![Codecov test
coverage](https://codecov.io/gh/antaldaniel/regions/branch/master/graph/badge.svg)](https://codecov.io/gh/antaldaniel/regions?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
“[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)”

<!-- badges: end -->

The goal of regions is to help the validation, imputation and boundary
tracking of regional statistical data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("antaldaniel/regions")
```

## Motivation

Most economic, social and environmental situations and developments have
a specific territorial dimension. Their statistical representations,
which generalize many individual data points by aggregating, averaging
or other means corresponds to a territory.

We are used to relatively constant territories in the forms of national
boundaries. While national boundaries do change almost every year
somewhere on Earth, lower level territorial units, such as
administrative divisions within a country, ranked below a province,
region, or state, are changing very frequently. Such changes make the
creation of time series, data panels, or data joins a very painful task
in practice. For example, the regional boundaries within the European
Union change on average every three years, so a panel or time series of
data has to account for about three boundary changes that raise various
comparability problems.

## Example

This is a basic example which shows you how to impute data from a larger
territorial unit, such as a national statistic, to lower territorial
units:

``` r
library(regions)
upstream <- data.frame ( country_code =  rep( "AU", 3),
                         year = c(2018:2020),
                         my_var  = c(10,12,11)
                       )

downstream <- australia_states

impute_down ( upstream_data  = upstream,
              downstream_data = downstream,
              country_var = "country_code",
              regional_code = "geo_code",
              values_var = "my_var",
              time_var = "year" )
#>    geo_code year                               geo_name country_code my_var
#> 1    AU-NSW 2018                  New South Wales state           AU     10
#> 2    AU-QLD 2018                       Queensland state           AU     10
#> 3     AU-SA 2018                  South Australia state           AU     10
#> 4    AU-TAS 2018                         Tasmania state           AU     10
#> 5    AU-VIC 2018                         Victoria state           AU     10
#> 6     AU-WA 2018                Western Australia state           AU     10
#> 7    AU-ACT 2018 Australian Capital Territory territory           AU     10
#> 8     AU-NT 2018           Northern Territory territory           AU     10
#> 9    AU-NSW 2019                  New South Wales state           AU     12
#> 10   AU-QLD 2019                       Queensland state           AU     12
#> 11    AU-SA 2019                  South Australia state           AU     12
#> 12   AU-TAS 2019                         Tasmania state           AU     12
#> 13   AU-VIC 2019                         Victoria state           AU     12
#> 14    AU-WA 2019                Western Australia state           AU     12
#> 15   AU-ACT 2019 Australian Capital Territory territory           AU     12
#> 16    AU-NT 2019           Northern Territory territory           AU     12
#> 17   AU-NSW 2020                  New South Wales state           AU     11
#> 18   AU-QLD 2020                       Queensland state           AU     11
#> 19    AU-SA 2020                  South Australia state           AU     11
#> 20   AU-TAS 2020                         Tasmania state           AU     11
#> 21   AU-VIC 2020                         Victoria state           AU     11
#> 22    AU-WA 2020                Western Australia state           AU     11
#> 23   AU-ACT 2020 Australian Capital Territory territory           AU     11
#> 24    AU-NT 2020           Northern Territory territory           AU     11
#>                    method
#> 1  imputed from AU actual
#> 2  imputed from AU actual
#> 3  imputed from AU actual
#> 4  imputed from AU actual
#> 5  imputed from AU actual
#> 6  imputed from AU actual
#> 7  imputed from AU actual
#> 8  imputed from AU actual
#> 9  imputed from AU actual
#> 10 imputed from AU actual
#> 11 imputed from AU actual
#> 12 imputed from AU actual
#> 13 imputed from AU actual
#> 14 imputed from AU actual
#> 15 imputed from AU actual
#> 16 imputed from AU actual
#> 17 imputed from AU actual
#> 18 imputed from AU actual
#> 19 imputed from AU actual
#> 20 imputed from AU actual
#> 21 imputed from AU actual
#> 22 imputed from AU actual
#> 23 imputed from AU actual
#> 24 imputed from AU actual
```

## Code of Conduct

Please note that the regions project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
