## Resubmission
This is a resubmission. In this version I have:

* Changed CRAN URL in README.md to canonical version.

In addition to previous submission:

* Updated version of tidytreatment prior to update tidybayes to v3.0.0 which has breaking changes
* This package has been tested with tested with `tidybayes` v2.3.1 (current) and v2.9.9.9000 (dev)

## Test environments
* local: ubuntu-18.04, R-4.1.0
* github-actions: ubuntu-18.04, R-devel, R-release, R-3.6, R-3.5
* github-actions: windows-latest, R-release, R-3.6
* github-actions: macOS-latest, R-release
* check-rhub: windows-server-2008, R-devel
* check-rhub: ubuntu-20.04.1, R-release
* check-rhub: fedora, R-devel

## R CMD check results
There were no ERRORs.

There was 1 WARNING:

* namespace ‘tidytreatment’ is not available and has been replaced

Only for ubuntu-18.04 (3.5)

There were 2 NOTEs:

* checking package dependencies ... NOTE Package which this enhances but not available for checking: ‘bartMachine’

Usage of bartMachine is optional and not required by package

* Found the following (possibly) invalid URLs: URL: https://projecteuclid.org/euclid.aoas/1380804800

URL working

## Downstream dependencies

There are currently no downstream dependencies for this package.
