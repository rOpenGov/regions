## Test environments
* local win install, R 4.0.0
0 errors v | 0 warnings v | 0 notes 

### Travis-CI
* ubuntu 14.04 (on travis-ci), R 4.0.0
* ubuntu 14.04 (on travis-ci), R 3.6.3 
0 errors v | 0 warnings v | 0 notes 

### win-builder.r-project.org


### rhub

* Fedora Linux, R-devel, on r_hub
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs or WARNINGs. 

Depending on the platform, I get various notes to use size reduction
of PDF. This should not throw a NOTE on CRAN.

## Resubmission With Reviewer Suggestions and Solutions Implemented
The reviewer saw 0.1.1, and this is versioned as 0.1.2. (see 
very minor changes in NEWS.md)

Reviewer had one suggestion: the author of the data table 
Google Mobility Report European Correspondence Table (in data)
should be added to the DESCRIPTION as contributor. (Istvan Zsoldos is
added.)

One extra functionality was added since. 

New function `impute_down_nuts` is handling EU typologies with only one
input (earlier `impute_down` is a more general case where the typology
has to be imputed, in this case it is read from internal data file.) 
Both versions of the function are extensively tested in the testthat 
infrastructure and pass all tests. An example with this function was added to the `Working With Regional, Sub-National Statistical Products` vignette.

Previous reviewer for 0.1.0. asked for more detailed documentation of 
returned objects which was added in 0.1.1. 
