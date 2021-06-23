# Tidy methods for Bayesian treatment effect models

<!-- badges: start -->
[![R-CMD-check](https://github.com/bonStats/tidytreatment/workflows/R-CMD-check/badge.svg)](https://github.com/bonStats/tidytreatment/actions)
<!-- badges: end -->

`tidytreatment` is an `R` package that provides functions for extracting tidy data from Bayesian treatment effect models, estimating treatment effects, and plotting useful summaries of these. This package closely follows the output style from the [tidybayes](https://github.com/mjskay/tidybayes) `R` package in order to use some functions provided by `tidybayes`.

The package currently supports the following models:

- `BART`: see [CRAN](https://cran.r-project.org/package=BART)
- `bartMachine`: see [CRAN](https://cran.r-project.org/package=bartMachine).
- `bcf`: see [CRAN](https://cran.r-project.org/package=bcf) (in development, see branch `bcf-hold` on github).

See `vignette("use-tidytreatment-BART")` for examples of usages.

## How to install

### Release - CRAN

Coming soon

### Dev - Github 

1. Make sure at least one of the above model fitting packages is installed.
2. In `R` make sure `remotes` is installed. Install with `install.packages("remotes")`.
    - For help: see the Rtools (windows) and Xcode (macOS) links on [this page](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites).
3. Run `remotes::install_github("bonStats/tidytreatment")`




