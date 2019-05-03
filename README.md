# Tidy methods for Bayesian treatment effect models

`tidytreatment` is an `R` package that provides functions for extracting tidy data from Bayesian treatment effect models, estimating treatment effects, and plotting useful summaries of these. This package closely follows the output style from the [tidybayes](https://github.com/mjskay/tidybayes) `R` package in order to use some functions provided by `tidybayes`.

## How to install

### Install for usage with `bartMachine` package (required for vignette too)

1. Install JDK and `rJava`
    - Here is a [guide](https://zhiyzuo.github.io/installation-rJava/) for macOS, and
    - a [guide](https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/) for Windows
2. In `R` make sure `devtools` is installed. Install with `install.packages("devtools")`.
    - For help: see the Rtools (windows) and Xcode (macOS) links on [this page](https://www.rstudio.com/products/rpackages/devtools/).
3. Install the following (if not already installed): `install.packages(c("knitr","rmarkdown"))`
4. Run `devtools::install_github("bonStats/tidytreatment", build_opts = c("--no-resave-data", "--no-manual"), dependencies = T )` 

### Install for other uses (currently none!)

1. In `R` make sure `devtools` is installed. Install with `install.packages("devtools")`.
    - For help: see the Rtools (windows) and Xcode (macOS) links on [this page](https://www.rstudio.com/products/rpackages/devtools/).
2. Run `devtools::install_github("bonStats/tidytreatment")`

## Package development aims

- `predicted_draws` and `fitted_draws` methods for `bartMachine` models. See `bartMachine` on [CRAN](https://cran.r-project.org/package=bartMachine)
    - ~~Implemented~~
    - Tested
- Conditional Average Treatment Effect summaries and plots
    - Implemented
    - Tested
