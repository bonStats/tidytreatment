# Tidy methods for Bayesian treatment effect models

<!-- badges: start -->
[![R-CMD-check](https://github.com/bonStats/tidytreatment/workflows/R-CMD-check/badge.svg)](https://github.com/bonStats/tidytreatment/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/tidytreatment)](https://CRAN.R-project.org/package=tidytreatment)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/tidytreatment)](https://cran.r-project.org/package=tidytreatment)
<!-- badges: end -->

`tidytreatment` is an `R` package that provides functions for extracting tidy data from Bayesian treatment effect models, estimating treatment effects, and plotting useful summaries of these. This package closely follows the output style from the [tidybayes](https://github.com/mjskay/tidybayes) `R` package in order to use some functions provided by `tidybayes`.

The package currently supports the following models:

- `BART`: see [CRAN](https://cran.r-project.org/package=BART)
- `bartMachine`: see [CRAN](https://cran.r-project.org/package=bartMachine).
- `bcf`: see [CRAN](https://cran.r-project.org/package=bcf) (in development, see branch `bcf-hold` on github)

in addition to models supported by [tidybayes](https://github.com/mjskay/tidybayes).

See this [HTML vignette](https://CRAN.R-project.org/package=tidytreatment/vignettes/use-tidytreatment-BART.html) or `vignette("use-tidytreatment-BART")` for examples of usage.

## How to install

### CRAN

Install the release version from CRAN with `install.packages("tidytreament")`.

### Github 

To install the latest development version:

1. Make sure at least one of the above model fitting packages is installed.
2. In `R` make sure `remotes` is installed. Install with `install.packages("remotes")`.
    - For help: see the Rtools (windows) and Xcode (macOS) links on [this page](https://support.rstudio.com/hc/en-us/articles/200486498-Package-Development-Prerequisites).
3. Run `remotes::install_github("bonStats/tidytreatment")`




