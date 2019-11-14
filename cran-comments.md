## Resubmission
This is a resubmission. In this version I have:

* As suggested by the reviewer, I have shortened the title in the DESCRIPTION
  file because exceeded 65 characters.
  
* Following the reviewer's advice we have developed more the Description field
  explaining what the pacakges does in more detail.

* I have also added missing \value tags in the .Rd files documenting two 
  functions.
  
## Test environments
* local Windows install, R 3.6.1
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
There were no ERRORs or WARNINGs. There should be 1 NOTE because this is the first time the package is submitted to CRAN. There is also a note because some
words could be misspelled in DESCRIPTION, but they are correctly spelled.

## Downstream dependencies
The changes made to this package have no effect in downstream dependencies.
