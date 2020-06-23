## Test environments
* local win install, R 4.0.0
0 errors v | 0 warnings v | 0 notes 

### Travis-CI
* ubuntu 14.04 (on travis-ci), R 4.0.0
* ubuntu 14.04 (on travis-ci), R 3.6.3 
0 errors v | 0 warnings v | 0 notes 

### rhub
* Fedora Linux, R-devel, on r_hub
* The Windows server on rhub does not have the vctrs >= 0.3.0, 
and is not able to handle (yet) the new dplyr 1.0.0; this is unrelated
to the 'regions' package.

## R CMD check results
There were no ERRORs or WARNINGs. 

Depending on the platform, I get various notes to use size reduction
of PDF. This should not throw a NOTE on CRAN.

## very small bug fix
This submission is a very small bug fix compared to the existing 0.1.3 CRAN version.  0.1.5 resolves a naming conflict in `validate_nuts_regions`. Furthermore, it handles some metadata exceptions for non-EU countries.  The metadata files of the package were updated with new entries [i.e. contain more geographical data], but they do not affect the functioning of codes.  