## Test environments
* local win install, R 4.1.0
0 errors v | 0 warnings v | 0 notes 

### Github 
* Microsoft Windows Server 2019, window-latest (release)
* ubuntu-20.04 (release)
* ubuntu-20.04 (devel)
* Mac OS X 10.15.7 (release)
0 errors v | 0 warnings v | 0 notes 

### rhub
* Fedora Linux, R-devel, clang, gfortran, on r_hub
* Ubuntu Linux 20.04.1 LTS, R-release, GCC, on r_hub

## R CMD check results
There were no ERRORs or WARNINGs. 

## This release is a minor bug fix
Apologies for a very quick new submission.

Version 0.1.8 is a essentially a bug fix for the recently released 0.1.7.  While 0.1.7 works are expected, there is a huge performance bottleneck in two functions which can be considered as a bug. 
* Removing a bottleneck from `validate_geo_code()` and `validate_nuts_regions()`.  The NUTS exceptions are essentially constants, and it was unnecessary to calculate with resource-intensive joins on large data frames each time these functions were running. They were moved to the `nuts_exceptions` dataset.