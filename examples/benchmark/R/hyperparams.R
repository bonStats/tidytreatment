# Shared hyperparameter target and per-engine adapters, so "baseline" fits
# across BART / dbarts / stan4bart / bartCause / stochtree are matched as
# closely as each package's own API allows.

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

# BART's own leaf-prior sd (tau), from BART::wbart/pbart source: continuous
# tau = (max(y)-min(y)) / (2*k*sqrt(ntree)); binary tau = 3 / (k*sqrt(ntree))
# (calibrated to the latent's +-3 range). Used directly for BART/dbarts;
# converted to a variance for stochtree in hp_to_stochtree_leaf_var() below.
hp_leaf_sd <- function(hp, y = NULL, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  if (outcome == "continuous") {
    stopifnot(!is.null(y))
    (max(y) - min(y)) / (2 * hp$k * sqrt(hp$n_trees))
  } else {
    3 / (hp$k * sqrt(hp$n_trees))
  }
}

# stochtree::bart()/bcf() standardize a continuous outcome internally
# (general_params$standardize = TRUE) and parameterise the leaf prior as a
# *variance* (sigma2_leaf_init = tau^2) on that scale, so tau (raw-y scale)
# needs dividing by var(y). Binary/probit outcomes are assumed not rescaled
# internally - a flagged approximation, not verified against stochtree's C++.
hp_to_stochtree_leaf_var <- function(hp, y = NULL, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  tau <- hp_leaf_sd(hp, y = y, outcome = outcome)
  y_var <- if (outcome == "continuous") stats::var(y) else 1
  (tau^2) / y_var
}

# BART/dbarts leave `sigest` at its own package default (NA): each computes
# lm(y ~ x.train)$sigma internally when n > p + 1, else sd(y), then (a) uses
# it as the chain's starting sigma and (b) calibrates the inverse-gamma
# scale lambda so that P(sigma < sigest) = sigquant under an nu = sigdf
# degrees-of-freedom prior. Both are replicated here: stochtree has no
# equivalent auto-calibration for either. Its prior default
# (sigma2_global_shape = sigma2_global_scale = 0) is an improper, essentially
# uninformative prior - confirmed to undercover badly at small n relative to
# BART/dbarts (see benchmark-prediction.qmd's Hyperparameters section). Its
# init default (sigma2_global_init = NULL) falls back to var(y) - the raw,
# unconditional variance, ignoring X entirely, rather than the regression-
# based sigest BART/dbarts start from. stochtree standardizes a continuous
# outcome internally, so both lambda and sigest^2 are divided by var(y)
# before conversion, matching hp_to_stochtree_leaf_var()'s own scale
# conversion. Not applicable to a binary/probit outcome - sigma2 there is
# fixed at 1, not sampled.
hp_to_stochtree_sigma2_prior <- function(hp, X, y) {
  n <- length(y)
  p <- ncol(X)
  sigest <- if (n > p + 1) {
    stats::sigma(stats::lm(y ~ ., data = cbind(y = y, X)))
  } else {
    stats::sd(y)
  }
  y_var <- stats::var(y)
  lambda <- sigest^2 * stats::qchisq(1 - hp$sigquant, df = hp$sigdf) / hp$sigdf
  lambda_std <- lambda / y_var

  list(
    sigma2_global_shape = hp$sigdf / 2,
    sigma2_global_scale = hp$sigdf * lambda_std / 2,
    sigma2_global_init = sigest^2 / y_var
  )
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

# ---- dbarts::bart2() (also stan4bart's bart_args, bartCause's args.rsp/args.trt) ----

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

hp_to_stochtree_bart <- function(hp, X, y = NULL, outcome = c("continuous", "binary"), num_gfr = 0, sample_leaf_var = FALSE) {
  outcome <- match.arg(outcome)

  general_params <- list()
  if (outcome == "binary") {
    general_params$outcome_model <- stochtree::OutcomeModel(outcome = "binary", link = "probit")
  } else {
    general_params <- c(general_params, hp_to_stochtree_sigma2_prior(hp, X, y))
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
      sigma2_leaf_init = hp_to_stochtree_leaf_var(hp, y = y, outcome = outcome),
      # stochtree's own default is TRUE - it would resample the leaf variance
      # every iteration from an internal IG(shape=3, scale=auto) hyperprior,
      # unlike BART/dbarts, where tau (this leaf variance's square root) is
      # calibrated once from k/ntree/range and held fixed for the whole
      # chain. FALSE (the baseline default) keeps sigma2_leaf_init above
      # fixed throughout, matching BART/dbarts as closely as stochtree's API
      # allows; sample_leaf_var = TRUE isolates just this one setting for
      # the "baseline+leaf_var" row.
      sample_sigma2_leaf = sample_leaf_var
    )
  )
}

# ---- stochtree::bcf() ---------------------------------------------------

# Baseline uses the *same* num_trees/alpha/beta/leaf-var for both forests,
# even though bcf()'s own defaults are asymmetric (250 prognostic / 100
# treatment-effect trees) - overridden for the "as similar as possible" baseline.
hp_to_stochtree_bcf <- function(hp, X, y = NULL, outcome = c("continuous", "binary"),
                                 num_gfr = 0, adaptive_coding = FALSE) {
  outcome <- match.arg(outcome)
  leaf_var <- hp_to_stochtree_leaf_var(hp, y = y, outcome = outcome)

  general_params <- list()
  if (outcome == "binary") {
    general_params$outcome_model <- stochtree::OutcomeModel(outcome = "binary", link = "probit")
  } else {
    general_params <- c(general_params, hp_to_stochtree_sigma2_prior(hp, X, y))
  }

  list(
    num_gfr = num_gfr,
    num_burnin = hp$burn,
    num_mcmc = hp$draws,
    general_params = c(general_params, list(adaptive_coding = adaptive_coding)),
    # sample_sigma2_leaf = FALSE for the same reason as hp_to_stochtree_bart():
    # keeps sigma2_leaf_init fixed for the whole chain, matching BART/dbarts.
    # bcf()'s own default already has this off for the treatment-effect
    # forest but on for the prognostic forest - set explicitly on both here
    # so baseline doesn't depend on that asymmetry.
    prognostic_forest_params = list(
      num_trees = hp$n_trees, alpha = hp$alpha, beta = hp$beta, sigma2_leaf_init = leaf_var,
      sample_sigma2_leaf = FALSE
    ),
    treatment_effect_forest_params = list(
      num_trees = hp$n_trees, alpha = hp$alpha, beta = hp$beta, sigma2_leaf_init = leaf_var,
      sample_sigma2_leaf = FALSE
    )
  )
}

# ---- Package-default hyperprior variants (prediction benchmark only) ----
#
# Each function below leaves the *hyperprior* settings (tree-structure prior,
# leaf-shrinkage prior, global error-variance prior) at that package's own
# defaults, instead of the baseline_hyperparams() translation, to isolate how
# much of baseline's advantage comes from the prior choices themselves. burn/
# draws/chains/num_gfr are not hyperprior settings - they're MCMC-budget and
# warm-start choices already varied by other means (n_reps, the "+gfr"
# variant) - so they stay fixed to baseline throughout, keeping this an
# apples-to-apples comparison of priors only.
#
# BART::wbart has no equivalent function here: its own defaults (ntree=200,
# k=2, power=2, base=0.95, sigdf=3, sigquant=0.9) are already identical to
# baseline_hyperparams() - baseline was defined to match them - so there is
# no distinct "package default" row to add for it.

# BART::pbart's own defaults differ from baseline only in ntree (50 vs 200) -
# k/power/base already equal baseline's values (they're BART::pbart's own
# defaults too), so are left unset here rather than restated.
hp_to_pbart_default <- function(hp) {
  list(
    ntree = 50L,
    nskip = hp$burn, ndpost = hp$draws
  )
}

# dbarts::bart2's own defaults differ from baseline in n.trees (75 vs 200)
# and k (NULL/auto-tuned vs fixed at 2) - the latter overlaps with, but is
# not the same combination as, the existing "+auto_k" variant (which keeps
# n.trees at 200). power/base/sigdf/sigquant already equal baseline's values
# (they're dbarts::bart2's own defaults too), so are left unset here rather
# than restated.
hp_to_dbarts_default <- function(hp) {
  list(
    n.trees = 75L, k = NULL,
    n.burn = hp$burn, n.samples = hp$draws, n.chains = hp$chains,
    keepTrees = TRUE
  )
}

# stochtree::bart()'s own num_trees/alpha/beta (200/0.95/2) already match
# baseline, so the only real difference is in what's left unset here: leaf
# variance defaults to NULL (sampled under an internal IG(shape=3, auto
# scale) hyperprior, rather than baseline's fixed tau-derived value), and for
# a continuous outcome, the global error-variance prior defaults to the
# improper sigma2_global_shape = sigma2_global_scale = 0 (see
# hp_to_stochtree_sigma2_prior()'s header comment).
hp_to_stochtree_bart_default <- function(hp, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  general_params <- list()
  if (outcome == "binary") {
    general_params$outcome_model <- stochtree::OutcomeModel(outcome = "binary", link = "probit")
  }
  list(
    # stochtree::bart()'s own default is num_gfr = 5, not 0 - deliberately
    # overridden here, unlike the hyperprior args below. num_gfr is a
    # warm-start setting, not a hyperprior, and is already varied on its own
    # via the "baseline+gfr" row, so leaving it at 5 here would conflate two
    # separate things (untuned priors and a different search procedure) in
    # one comparison.
    num_gfr = 0,
    num_burnin = hp$burn,
    num_mcmc = hp$draws,
    general_params = general_params,
    mean_forest_params = list()
  )
}
