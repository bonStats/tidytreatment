# Shared hyperparameter target and per-engine adapters, so "baseline" fits
# across BART / stan4bart / bartCause / stochtree are matched as closely as
# each package's own API allows. See examples/benchmark/README.md (plan) for
# the rationale behind each choice.

baseline_hyperparams <- function() {
  list(
    n_trees  = 200,   # BART::wbart's own default; dbarts defaults to 75,
                       # BART::pbart defaults to 50 - both overridden to 200.
    alpha    = 0.95,   # tree split prior: BART's "base" / stochtree's "alpha"
    beta     = 2,      # tree split prior: BART's "power" / stochtree's "beta"
    k        = 2,      # leaf shrinkage; dbarts defaults to NULL (auto-tuned)
                        # - the "+auto-k" variant restores that behaviour.
    sigdf    = 3,
    sigquant = 0.9,
    burn     = 1000,
    draws    = 1000,
    chains   = 1
  )
}

# BART's own leaf-prior sd (tau, in BART's notation), reverse-engineered from
# BART::wbart / BART::pbart source: for a continuous outcome
# tau = (max(y)-min(y)) / (2*k*sqrt(ntree)); for a probit/binary outcome
# tau = 3 / (k*sqrt(ntree)) (calibrated to the latent's implicit +-3 range,
# not the raw y range). Used directly for BART/dbarts (which take k itself)
# and converted for stochtree (which parameterises the leaf prior by variance
# rather than by k) in hp_to_stochtree_leaf_var() below.
hp_leaf_sd <- function(hp, y = NULL, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  if (outcome == "continuous") {
    stopifnot(!is.null(y))
    (max(y) - min(y)) / (2 * hp$k * sqrt(hp$n_trees))
  } else {
    3 / (hp$k * sqrt(hp$n_trees))
  }
}

# stochtree::bart()/bcf() standardize a continuous outcome internally by
# default (general_params$standardize = TRUE) and parameterise the leaf prior
# on that standardized scale via sigma2_leaf_init (a *variance*, i.e. tau^2).
# So BART's tau (computed on the raw y scale) needs dividing by var(y) before
# use here. Binary/probit outcomes are assumed not to be rescaled internally
# (the latent scale is already the fixed +-3-ish range BART's own pbart
# calibration assumes) - this is a flagged approximation, not verified against
# stochtree's C++ internals; the smoke test in this file checks it at least
# produces a sane, finite value and a model that fits without error.
hp_to_stochtree_leaf_var <- function(hp, y = NULL, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  tau <- hp_leaf_sd(hp, y = y, outcome = outcome)
  y_var <- if (outcome == "continuous") stats::var(y) else 1
  (tau^2) / y_var
}

# ---- BART package ----------------------------------------------------

hp_to_wbart <- function(hp) {
  list(
    ntree = hp$n_trees, k = hp$k, power = hp$beta, base = hp$alpha,
    sigdf = hp$sigdf, sigquant = hp$sigquant,
    nskip = hp$burn, ndpost = hp$draws
  )
}

hp_to_pbart <- function(hp) {
  list(
    ntree = hp$n_trees, k = hp$k, power = hp$beta, base = hp$alpha,
    nskip = hp$burn, ndpost = hp$draws
  )
}

# ---- dbarts (stan4bart's bart_args / bartCause's args.rsp & args.trt) ----

hp_to_dbarts <- function(hp, auto_k = FALSE) {
  list(
    n.trees = hp$n_trees,
    k = if (auto_k) NULL else hp$k,
    power = hp$beta,
    base = hp$alpha,
    sigdf = hp$sigdf,
    sigquant = hp$sigquant,
    n.burn = hp$burn,
    n.samples = hp$draws,
    n.chains = hp$chains,
    keepTrees = TRUE # required for out-of-sample predict() later
  )
}

# ---- stochtree::bart() -------------------------------------------------

hp_to_stochtree_bart <- function(hp, y = NULL, outcome = c("continuous", "binary"), num_gfr = 0) {
  outcome <- match.arg(outcome)

  general_params <- list()
  if (outcome == "binary") {
    general_params$outcome_model <- stochtree::OutcomeModel(outcome = "binary", link = "probit")
  }

  list(
    num_gfr = num_gfr,
    num_burnin = hp$burn,
    num_mcmc = hp$draws,
    general_params = general_params,
    mean_forest_params = list(
      num_trees = hp$n_trees,
      alpha = hp$alpha,
      beta = hp$beta,
      sigma2_leaf_init = hp_to_stochtree_leaf_var(hp, y = y, outcome = outcome)
    )
  )
}

# ---- stochtree::bcf() ---------------------------------------------------

# Baseline deliberately uses the *same* num_trees/alpha/beta/leaf-var for both
# the prognostic and treatment-effect forests (matching the single shared
# n_trees used everywhere else), even though bcf()'s own defaults are
# asymmetric (250 prognostic / 100 treatment-effect trees) - that asymmetry
# reflects real modelling practice but isn't something the other engines can
# express, so it's overridden for the "as similar as possible" baseline.
hp_to_stochtree_bcf <- function(hp, y = NULL, outcome = c("continuous", "binary"),
                                 num_gfr = 0, adaptive_coding = FALSE) {
  outcome <- match.arg(outcome)
  leaf_var <- hp_to_stochtree_leaf_var(hp, y = y, outcome = outcome)

  general_params <- list()
  if (outcome == "binary") {
    general_params$outcome_model <- stochtree::OutcomeModel(outcome = "binary", link = "probit")
  }

  list(
    num_gfr = num_gfr,
    num_burnin = hp$burn,
    num_mcmc = hp$draws,
    general_params = c(general_params, list(adaptive_coding = adaptive_coding)),
    prognostic_forest_params = list(
      num_trees = hp$n_trees, alpha = hp$alpha, beta = hp$beta, sigma2_leaf_init = leaf_var
    ),
    treatment_effect_forest_params = list(
      num_trees = hp$n_trees, alpha = hp$alpha, beta = hp$beta, sigma2_leaf_init = leaf_var
    )
  )
}
