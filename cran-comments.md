## Test environments
* local win install, R 4.0.0
* ubuntu 14.04 (on travis-ci), R 4.0.0
* ubuntu 14.04 (on travis-ci), R 3.6.3 
* Fedora Linux, R-devel, on r_hub
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs. 

Dependingn on the platform, I get various notes to use size reduction
of PDF.

## Resubmission With Reviewer Suggestions and Solutions Implemented
The reviewer saw 0.1.0, and this is versioned as 0.1.0.0002. (see 
very minor changes in NEWS.md)

### Further documentation
Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. 

structure of the output (class) and also what the output means.

(If a function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)

* magrittr pipe operator was re-exported for convenience in packages, and See \code{magrittr::\link[magrittr]{\%>\%}} for details was added to .Rd

* validate_nuts_countries() is extended with more detail.

* recode_nuts() is extended both with the description and the value fields.

* get_country_code() is extended with return class information, i.e. character vector.

* Had already been there and not changed: validate_nuts_regions.

* data-federal change is extended with Source and URL

* data-nuts_recoded, data-nuts_changes, data-all_valid_nuts_codes are
better connected with each other and the eventual source is highlighted 
in all three datasets.

### Authors

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>

I am planning to make a submission to the Journal of Statistical Software 
after adding some functionality in 0.2.0 and extensively using the package
with real datasets.  I am the author of all methods, except for a correspondence table that is added since the 0.1.0 as 0.1.0.0001 with the
contributor's name documented. [It is a correspondence table for a future
feature to translate Google's own typology to EU typologies, and the method itself is not yet implemented.]