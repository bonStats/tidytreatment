# Tidy methods for Bayesian treatment effect models

`tidytreatment` is an `R` package that provides functions for extracting tidy data from Bayesian treatment effect models, estimating treatment effects, and plotting useful summaries of these. This package closely follows the output style from the [tidybayes](https://github.com/mjskay/tidybayes) `R` package in order to use some functions provided by `tidybayes`.

## How to install

1. In `R` make sure `devtools` is installed. Install with `install.packages("devtools")`.
2. Then run `devtools::install_github("bonStats/tidytreatment", build_vignettes=TRUE)` .
3. To look at the current vignette try `vignette("use-tidytreatment", package = "tidytreatment")`

## Package development aims

- `predicted_draws` and `fitted_draws` methods for `bartMachine` models. See `bartMachine` on [CRAN](https://cran.r-project.org/package=bartMachine)
    - ~~Implemented~~
    - Tested
- Conditional Average Treatment Effect summaries and plots
    - Implemented
    - Tested
