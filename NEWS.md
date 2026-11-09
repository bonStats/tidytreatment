# tidytreatment (development version)

# tidytreatment

## tidytreatment 1.0.0

* New model support: `stochtree::bart()` (continuous and binary outcomes) and `stochtree::bcf()` (Bayesian Causal Forest), including group-level random effects (random intercepts) for `bcf()` models.
* `covariate_importance()` now returns a consistent two-column schema across every supported engine (`BART`, `dbarts`, `bartMachine`, `stan4bart`, `stochtree::bart`, `stochtree::bcf`): a raw split-count column (`inclusion`) and a per-tree inclusion-probability column (`avg_inclusion = inclusion / (iterations * trees)`). Previously the normalisation differed by engine - for example `stochtree::bcf` reported a share-of-total-splits figure rather than a per-tree probability.
* `covariate_importance.bcfmodel()`'s `forest` argument now takes `"treatment"` instead of `"treatment_effect"` to select the treatment-effect forest.
* `epred_draws()` now consistently defaults to the response scale (resolved from the model's own outcome-type metadata for `stochtree` models), and `include_newdata` now defaults to `FALSE` everywhere - it was previously `TRUE` for `BART`-package models - since most underlying model objects don't store the data they were fitted on. A warning now explains the cost of repeating `newdata` once per posterior draw when `include_newdata = TRUE` is requested anyway.
* Scale terminology standardised on `"linear"` and `"probability"` for the `scale` argument of `epred_draws()`/`predicted_draws()`/`residual_draws()` (previously a mix of `"real"`/`"probit"`/`"prob"` across methods).
* Fixed `treatment_effects.bartcFit()`'s column naming for treatment-effect draws.
* `BART::mbart`/`mbart2` (multinomial outcomes) are explicitly unsupported: calling tidytreatment methods on them now errors informatively, since their per-category tree representation is incompatible with this package's machinery, rather than behaving incorrectly.
* Added multi-chain handling where supported by the underlying engine.
* Various correctness fixes and new serialization tests for `stochtree::bcf` models.

## tidytreatment 0.3.2

* Change `fitted_draws` to `epred_draws` as per https://mjskay.github.io/tidybayes/news/index.html#tidybayes-300
* Remove dependence on `readr::read_table2` which has been deprecated.

## tidytreatment 0.3.1

* Now supports bartCause and stan4bart
* New vignette
* Patch issues with dependencies
* Update on CRAN

## tidytreatment 0.3.0.1

* Now supports bartCause and stan4bart (github versions)
* Will move to CRAN soon

## tidytreatment 0.2.2

* Updates to handle changes in ggdist required arguments.

## tidytreatment 0.2.1

* Updates to handle breaking changes in upcoming `tidybayes` 3.0.0 release. 
* Package tested with `tidybayes` 2.3.1 and 2.9.9.9000

## tidytreatment 0.2.0

* First CRAN submission

* Implementation of (average) treatment effects, common support, tidy posterior draws, and (simple) covariate importance functions.

* For fitted models from the BART and bartMachine packages. 
