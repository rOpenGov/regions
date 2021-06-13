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

## This release is a minor release
This submission is a minor relase that only contains good coding practices, compatibility with tidyverse 1.0+ and documentation improvements. 

This is a resubmission, because while all Github, rhub, and local tests passed, on CRAN one vignette did not build because of a short lapse of the Eurostat server (the data was not downloaded from the API.)  This affected only one build.  To avoid this happening, we placed 
the `regional_rd_personnel`, a small, filtered subset of the Eurostat dataset in question among the saved datasets.


