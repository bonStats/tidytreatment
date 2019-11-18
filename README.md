# Tidy methods for Bayesian treatment effect models

`tidytreatment` is an `R` package that provides functions for extracting tidy data from Bayesian treatment effect models, estimating treatment effects, and plotting useful summaries of these. This package closely follows the output style from the [tidybayes](https://github.com/mjskay/tidybayes) `R` package in order to use some functions provided by `tidybayes`.

The package currently supports the following models:

- `BART`: see [CRAN](https://cran.r-project.org/package=BART)
- `bcf`: see [CRAN](https://cran.r-project.org/package=bcf). Limited functionality, must use `tidytreatment::bcf2` to use.
- `bartMachine`: see [CRAN](https://cran.r-project.org/package=bartMachine).

## How to install

1. Make sure at least one of the above model fitting packages is installed. If using with `bartMachine` see the additional prerequisites below.
2. In `R` make sure `devtools` is installed. Install with `install.packages("devtools")`.
    - For help: see the Rtools (windows) and Xcode (macOS) links on [this page](https://www.rstudio.com/products/rpackages/devtools/).
3. Run `devtools::install_github("bonStats/tidytreatment")`

### Prerequisites for usage with `bartMachine` package (required for vignette too)

1. Install JDK and `rJava`
    - Here is a [macOS guide](https://zhiyzuo.github.io/installation-rJava/), and
    - a [Windows guide](https://cimentadaj.github.io/blog/2018-05-25-installing-rjava-on-windows-10/installing-rjava-on-windows-10/)
2. In `R` make sure `devtools` is installed. Install with `install.packages("devtools")`.
    - For help: see the Rtools (windows) and Xcode (macOS) links on [this page](https://www.rstudio.com/products/rpackages/devtools/).
3. Install the following (if not already installed): `install.packages(c("knitr","rmarkdown"))`
4. Run `devtools::install_github("bonStats/tidytreatment", build_opts = c("--no-resave-data", "--no-manual"), dependencies = T )` 

## Package development aims

- Methods for `BART` models: `wbart`, `pbart`, `lbart`, `mbart`, `mbart2` (others to come).
    - ~~Implement `fitted_draws`~~:
    - ~~Implement `predicted_draws`~~
    - ~~Implement `residual_draws`~~
    - ~~`treatment_effects`~~: Conditional (Average) Treatment Effects
    - ~~Common support testing~~
    - Unit testing
- Methods for `bcf` models.
    - ~~Implement `fitted_draws`~~ 
    - Implement `predicted_draws`
    - Implement `residual_draws`
    - ~~`treatment_effects`~~
    - Common support testing
    - Unit testing
- Methods for `bartMachine` models.
    - ~~Implement `fitted_draws`~~:
    - ~~Implement `predicted_draws`~~
    - ~~Implement `residual_draws`~~
    - ~~`treatment_effects`~~: Conditional (Average) Treatment Effects
    - ~~Common support testing~~
    - Unit testing 

