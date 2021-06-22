## Resubmission
This is a resubmission. In this version I have:

* Converted all instances of 'T' and 'F' to 'TRUE' and 'FALSE'

* Removed the dependence on 'installed.packages()'

* Fixed "Possibly mis-spelled words in DESCRIPTION: tidybayes (8:202)"

## Test environments
* local: ubuntu-16.04, R-4.1.0
* github-actions: ubuntu-18.04, R-devel, R-release, R-3.6, R-3.5
* github-actions: windows-latest, R-release, R-3.6
* github-actions: macOS-latest, R-release

## R CMD check results
There were no ERRORs.

There was 1 WARNING:

* namespace ‘tidytreatment’ is not available and has been replaced

Only for ubuntu-18.04 (3.5)

There were 2 NOTEs:

* checking package dependencies ... NOTE Package which this enhances but not available for checking: ‘bartMachine’

Usage of bartMachine is optional and not required by package

* Found the following (possibly) invalid URLs: URL: https://projecteuclid.org/euclid.aoas/1380804800 From: man/has_common_support.Rd man/simulate_su_hill_data.Rd Status: 500 Message: Internal Server Error

url working

## Downstream dependencies

There are currently no downstream dependencies for this package.
