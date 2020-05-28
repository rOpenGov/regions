
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regions

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/antaldaniel/regions/branch/master/graph/badge.svg)](https://codecov.io/gh/antaldaniel/regions?branch=master)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
“[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)”
[![Travis build
status](https://travis-ci.org/antaldaniel/regions.svg?branch=master)](https://travis-ci.org/antaldaniel/regions)
“[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/regions)](https://cran.r-project.org/package=regions)”
“[![CRAN\_Status\_Badge\_version\_last\_release](https://www.r-pkg.org/badges/version-last-release/regions)](https://cran.r-project.org/package=regions)”
“[![metacran
downloads](https://cranlogs.r-pkg.org/badges/regions)](https://cran.r-project.org/package=regions)”
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3825696.svg)](https://doi.org/10.5281/zenodo.3825696)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)
<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("rOpenGov/regions")
```

or the released version from CRAN:

``` r
install.packages("devtools")
```

You can review the complete package documentation on
[regions.danielantal.eu](http://regions.danielantal.eu/). If you find
any problems with the code, please raise an issue on
[Github](https://github.com/antaldaniel/regions). Pull requests are
welcome if you agree with the [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html)

If you use `regions` in your work, please [cite the
package](https://doi.org/10.5281/zenodo.3825696).

## Motivation

Working with sub-national statistics has many benefits. In policymaking
or in social sciences, it is a common practice to compare national
statistics, which can be hugely misleading. The United States of
America, the Federal Republic of Germany, Slovakia and Luxembourg are
all countries, but they differ vastly in size and social homogeneity.
Comparing Slovakia and Luxembourg to the federal states or even regions
within Germany, or the states of Germany and the United States can
provide more adequate insights. Statistically, the similarity of the
aggregation level and high number of observations can allow more precise
control of model parameters and errors. The advantages of switching from
a national level of the analysis to a sub-national level comes with a
huge price in data processing, validation and imputation. The package
Regions aims to help this process.

This package is an offspring of the
[eurostat](http://ropengov.github.io/eurostat/) package on
[rOpenGov](http://ropengov.github.io/). It started as a tool to validate
and re-code regional Eurostat statistics, but it aims to be a general
solution for all sub-national statistics. It will be developed parallel
with other rOpenGov packages.

## Sub-national Statistics Have Many Challenges

  - **Frequent boundary changes**: as opposed to national boundaries,
    the territorial units, typologies are often change, and this makes
    the validation and recoding of observation necessary across time.
    For example, in the European Union, sub-national typologies change
    about every three years and you have to make sure that you compare
    the right French region in time, or, if you can make the time-wise
    comparison at all.

  - **Hierarchical aggregation and special imputation**: missingness is
    very frequent in sub-national statistics, because they are created
    with a serious time-lag compared to national ones, and because they
    are often not back-casted after boundary changes. You cannot use
    standard imputation algorithms because the observations are not
    similarly aggregated or averaged. Often, the information is
    seemingly missing, and it is present with an obsolete typology code.

## Package functionality

  - Generic vocabulary translation and joining functions for
    geographically coded data
  - Keeping track of the boundary changes within the European Union
    between 1999-2021
  - Vocabulary translation and joining functions for standardized
    European Union statistics
  - Vocabulary translation for the `ISO-3166-2` based Google data and
    the European Union
  - Imputation functions from higher aggregation hierarchy levels to
    lower ones, for example from `NUTS1` to `NUTS2` or from `ISO-3166-1`
    to `ISO-3166-2` (impute down)
  - Imputation functions from lower hierarchy levels to higher ones
    (impute up)
  - Aggregation function from lower hierarchy levels to higher ones, for
    example from NUTS3 to `NUTS1` or from `ISO-3166-2` to `ISO-3166-1`
    (aggregate; under development)
  - Disaggregation functions from higher hierarchy levels to lower ones,
    again, for example from `NUTS1` to `NUTS2` or from `ISO-3166-1` to
    `ISO-3166-2` (disaggregate; under development)

## Example

This is a basic example which shows you how to impute data from a larger
territorial unit, such as a national statistic, to lower territorial
units:

``` r
library(regions)

upstream <- data.frame ( country_code =  rep( "AU", 2),
                         year = c(2019:2020),
                         my_var  = c(10,12)
                       )

downstream <- australia_states

imputed <- impute_down ( upstream_data  = upstream,
              downstream_data = downstream,
              country_var = "country_code",
              regional_code = "geo_code",
              values_var = "my_var",
              time_var = "year" )

knitr::kable(imputed)
```

| geo\_code | year | geo\_name                              | country\_code | my\_var | method                 |
| :-------- | ---: | :------------------------------------- | :------------ | ------: | :--------------------- |
| AU-NSW    | 2019 | New South Wales state                  | AU            |      10 | imputed from AU actual |
| AU-QLD    | 2019 | Queensland state                       | AU            |      10 | imputed from AU actual |
| AU-SA     | 2019 | South Australia state                  | AU            |      10 | imputed from AU actual |
| AU-TAS    | 2019 | Tasmania state                         | AU            |      10 | imputed from AU actual |
| AU-VIC    | 2019 | Victoria state                         | AU            |      10 | imputed from AU actual |
| AU-WA     | 2019 | Western Australia state                | AU            |      10 | imputed from AU actual |
| AU-ACT    | 2019 | Australian Capital Territory territory | AU            |      10 | imputed from AU actual |
| AU-NT     | 2019 | Northern Territory territory           | AU            |      10 | imputed from AU actual |
| AU-NSW    | 2020 | New South Wales state                  | AU            |      12 | imputed from AU actual |
| AU-QLD    | 2020 | Queensland state                       | AU            |      12 | imputed from AU actual |
| AU-SA     | 2020 | South Australia state                  | AU            |      12 | imputed from AU actual |
| AU-TAS    | 2020 | Tasmania state                         | AU            |      12 | imputed from AU actual |
| AU-VIC    | 2020 | Victoria state                         | AU            |      12 | imputed from AU actual |
| AU-WA     | 2020 | Western Australia state                | AU            |      12 | imputed from AU actual |
| AU-ACT    | 2020 | Australian Capital Territory territory | AU            |      12 | imputed from AU actual |
| AU-NT     | 2020 | Northern Territory territory           | AU            |      12 | imputed from AU actual |

## Code of Conduct

Please note that the regions project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
