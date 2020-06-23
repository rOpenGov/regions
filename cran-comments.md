## Test environments
* local win install, R 4.0.0
0 errors v | 0 warnings v | 0 notes 

### Travis-CI
* ubuntu 14.04 (on travis-ci), R 4.0.0
* ubuntu 14.04 (on travis-ci), R 3.6.3 
0 errors v | 0 warnings v | 0 notes 

### rhub
* Fedora Linux, R-devel, on r_hub
* Ubuntu Linux 16.04 LTS, R-devel, on r_hub
* The Windows Server 2008 R2 SP1, R-devel does not have knitr and cannot build the vignettes, but this is not related to regions.

## R CMD check results
There were no ERRORs or WARNINGs. 

## This release is very small bug fix
This submission is a very small bug fix compared to the existing 0.1.3 CRAN version.  0.1.5 resolves a naming conflict in `validate_nuts_regions`. Furthermore, it handles some metadata exceptions for non-EU countries.  The metadata files of the package were updated with new entries [i.e. contain more geographical data], but they do not affect the functioning of codes. An exception wordlist was added to the spell checker.