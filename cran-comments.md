## New Submission

* Updated version of tidytreatment to address problems on CRAN Package Check Results 

## Test environments

* local: ubuntu-20.04, R-4.1.2
* github-actions: ubuntu-18.04, R-devel, R-release, R-3.6, R-3.5
* github-actions: windows-latest, R-release, R-3.6
* github-actions: macOS-latest, R-release
* check-rhub: windows-server-2022, R-devel
* check-rhub: ubuntu-20.04, R-release
* check-rhub: fedora, R-devel

## R CMD check results

There were no ERRORs.

There was 1 WARNING(s):

* namespace ‘tidytreatment’ is not available and has been replaced
Only for ubuntu-18.04 (3.5)

There was 2 NOTE(s):

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
* checking package dependencies ... NOTE Package which this enhances but not available for checking: ‘bartMachine’
Usage of bartMachine is optional and not required by package

## Downstream dependencies

There are currently no downstream dependencies for this package.
