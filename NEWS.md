# regions 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Preparing a release candidate with Travis-CI integration.

# regions 0.1.0
* Submitted to CRAN

# regions 0.1.0.0001
* Adding the `google_nuts_matchtable` by Istvan Zsoldos for joining data from the Google Mobility Reports with Eurostat datasets.

# regions 0.1.1
* Adding further documentation items at request of CRAN reviewer (Thanks!).

# regions 0.1.2 
* New function `impute_down_nuts` for less general cases, i.e. EU NUTS imputations.
* Contributor attribution consistent in DESCRIPTION and source file. 
* Release candidate on CRAN.

# regions 0.1.3 
* Further improvements in the Google typology: adding the United Kingdom, Portugal, Greece, Malta, parts of Lativa, Italy and Réunion.
* `recode_nuts` slightly alterred to follow changes in dependency `dplyr`. 

# regions 0.1.4
* Documentation improvements.
* `validate_country_nuts_countries` is now follows dplyr 1.0, this makes the code more readable.
* `validate_nuts_regions` is validating non-EU NUTS-like regions as valid if they will be added to `NUTS2021`. These regional codes, while legally not part of the `NUTS2016` typology, are valid and can be placed on the maps created by [EuroGeographics maps provided by Eurostat](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts).
* Italy, Portugal, the United Kingdom, Estonia, Slovenia,Latvia pseudo-NUTS3 codes in `google_nuts_matchtable`.
* New correspondence table for converstion between Local Administration Units (LAUs) and NUTS within the European Union and some other European countries.