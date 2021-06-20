# regions 0.1.8
* Removing a bottleneck from `validate_geo_code()` and `validate_nuts_regions()`.  The NUTS exceptions are essentially constants, and it was unnecessary to calculate them each time these functions were running. They were moved to the `nuts_exceptions` dataset.
* Citation info was given the the vignette articles.
* Some error messages in assertions were made clearer are more consistent.

# regions 0.1.7
* Added the `regional_rd_personnel` dataset and the `Maping Regional Data, Maping Metadata Problems` vignette.
* A new function, `validate_parameters()` takes over several parameter validations. The aim of this function is to provide consistent error messages, and thorough validation of function parameters whenever they must conform to a closed vocabulary or parameter range.
* This version is released on CRAN.
* We started building an experimental APIs data is running regions regularly and improving known statistical data sources.  See: [Digital Music Observatory](https://music.dataobservatory.eu/), [Green Deal Data Observatory](https://greendeal.dataobservatory.eu/), [Economy Data Observatory](https://economy.dataobservatory.eu/).

# regions 0.1.6
* Exception handling partly moved to assertthat.
* New test_geo_code of for performance enhancement. 
* Changing non-standard evaluation to dplyr 1.0+
* Master repo moving to [rOpenGov](https://github.com/ropengov/).
* Rewritten README.

# regions 0.1.5
* Correction of a small bug with input data frames that already have columns that the validation result should contain. 
* Wordlist added for spell checking with geographical exceptions.

# regions 0.1.4
* Documentation improvements.
* `validate_country_nuts_countries` is now follows dplyr 1.0, this makes the code more readable.
* `validate_nuts_regions` is validating non-EU NUTS-like regions as valid if they will be added to `NUTS2021`. These regional codes, while legally not part of the `NUTS2016` typology, are valid and can be placed on the maps created by [EuroGeographics maps provided by Eurostat](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts).
* Italy, Portugal, the United Kingdom, Estonia, Slovenia,Latvia pseudo-NUTS3 codes in `google_nuts_matchtable`.
* New correspondence table for conversion between Local Administration Units (LAUs) and NUTS within the European Union and some other European countries.

# regions 0.1.3 
* Further improvements in the Google typology: adding the United Kingdom, Portugal, Greece, Malta, parts of Latvia, Italy and Reunion.
* `recode_nuts` slightly altered to follow changes in dependency `dplyr`. 

# regions 0.1.2 
* New function `impute_down_nuts` for less general cases, i.e. EU NUTS imputations.
* Contributor attribution consistent in DESCRIPTION and source file. 
* Release candidate on CRAN.

# regions 0.1.0.0001
* Adding the `google_nuts_matchtable` by Istvan Zsoldos for joining data from the Google Mobility Reports with Eurostat datasets.

# regions 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Preparing a release candidate with Travis-CI integration.