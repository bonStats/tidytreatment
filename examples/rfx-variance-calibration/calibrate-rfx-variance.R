# Scoping/demo: calibrating stochtree's random-effects (intercept-only)
# variance prior the same way examples/benchmark/R/hyperparams.R already
# calibrates the global error-variance prior (sigma2_global_shape/scale)
# against BART/dbarts's own sigest/sigdf/sigquant convention.
#
# ---------------------------------------------------------------------------
# Background (from https://stochtree.ai/about/bart.html /
# https://stochtree.ai/about/bcf.html, and deparsing stochtree::bart's body -
# the docs don't mention internal scaling, this was confirmed from source):
#
# stochtree's random-effects model uses Gelman et al.'s "redundant
# parameterization": the group effect is gamma_j = alpha * xi_j, with
#   alpha    ~ N(mu_alpha,   sigma2_alpha)      (sigma2_alpha fixed at 1 by
#                                                 default, NOT resampled)
#   xi_j     ~ N(mu_xi_j,    sigma2_xi)
#   sigma2_xi ~ IG(a_xi, b_xi)                  (resampled every iteration)
# `random_effects_params` exposes a_xi/b_xi as `variance_prior_shape` /
# `variance_prior_scale`, both defaulting to the fixed constants 1 and 1 -
# i.e. NOT calibrated to the data at all, unlike the global-sigma prior.
#
# For `model_spec = "intercept_only"` (the only case this script addresses),
# gamma_j is the group intercept itself.
#
# Internal scaling: stochtree standardizes y internally before sampling and
# rescales rfx predictions by `* y_std_train` - exactly the same adjustment
# hp_to_stochtree_sigma2_prior() already applies via `/ y_var`.
#
# Identification note this calibration relies on: since sigma2_alpha is
# fixed at its default of 1 (not given its own hyperprior/resampling),
#   E[gamma_j^2] = E[alpha^2] * E[xi_j^2 | sigma2_xi] = sigma2_alpha * sigma2_xi = sigma2_xi.
# In other words, as long as `working_parameter_prior_cov` (sigma2_alpha) is
# left at stochtree's default, sigma2_xi *is* the marginal prior variance of
# the random intercept itself - the redundant parameterization is a
# mixing-speed device (Gelman et al. 2013 PX-DA) that leaves the induced
# marginal prior on gamma unchanged, which is what licenses calibrating
# variance_prior_shape/scale directly against an estimated between-group
# variance, the same way sigma2_global_shape/scale is calibrated against
# sigest^2. This assumption is NOT re-derived or checked at runtime - it's a
# precondition for treating this calibration as valid.
# ---------------------------------------------------------------------------

suppressMessages({
  library(stochtree)
})

# =========================== 1. The starting point ==========================
# Copied verbatim from the existing examples/benchmark/R/hyperparams.R
# (see that file for the fuller header comments on BART/dbarts's own
# auto-calibration rule this mirrors). Used below as the baseline
# global-sigma calibration to compare the new rfx calibration against -
# unchanged, not part of what this script is proposing.

compute_sigest <- function(X, y, group = NULL) {
  n <- length(y)
  p <- ncol(X)
  dat <- cbind(y = y, X)

  if (is.null(group)) {
    return(if (n > p + 1) stats::sigma(stats::lm(y ~ ., data = dat)) else stats::sd(y))
  }

  dat$.group <- factor(group)
  if (n <= p + 1) return(stats::sd(y))

  if (requireNamespace("lme4", quietly = TRUE)) {
    fml <- stats::as.formula(paste("y ~", paste(colnames(X), collapse = " + "), "+ (1 | .group)"))
    fit <- try(suppressMessages(suppressWarnings(lme4::lmer(fml, data = dat))), silent = TRUE)
    if (!inherits(fit, "try-error")) return(stats::sigma(fit))
  }

  # Fallback: absorb the groups as fixed effects, counting the dummies in the
  # degrees-of-freedom guard.
  p_fe <- p + nlevels(dat$.group) - 1
  if (n > p_fe + 1) stats::sigma(stats::lm(y ~ ., data = dat)) else stats::sd(y)
}

hp_to_stochtree_sigma2_prior <- function(hp, X, y, group = NULL) {
  sigest <- compute_sigest(X, y, group = group)
  y_var <- stats::var(y)
  lambda <- sigest^2 * stats::qchisq(1 - hp$sigquant, df = hp$sigdf) / hp$sigdf
  lambda_std <- lambda / y_var

  list(
    sigma2_global_shape = hp$sigdf / 2,
    sigma2_global_scale = hp$sigdf * lambda_std / 2,
    sigma2_global_init = sigest^2 / y_var
  )
}

# ===================== 2. New: the rfx variance calibration =================
#
# estimate_sigma_G() mirrors compute_sigest()'s own two-path structure
# (lme4 REML / fixed-effect-dummy fallback) but targets the *group-level*
# (random-intercept) standard deviation instead of the residual standard
# deviation, and is deliberately conservative about when to trust that
# estimate.
#
# Per-user decision: rather than flooring a degenerate estimate to some
# fabricated heuristic value, this breaks loudly (a `warning()` naming the
# reason) and returns NULL. Callers must treat NULL as "do not override
# stochtree's own default rfx variance prior" - there is no safe synthetic
# fallback value for a quantity this is unable to estimate.
#
# Two ways the estimate is judged degenerate:
#   1. Too few groups (< min_groups) for a variance component to be
#      meaningfully estimated at all - checked before fitting anything.
#   2. A zero/boundary point estimate: REML can push the group variance
#      component to exactly 0, and the no-lme4 ANOVA fallback's
#      (MSB - MSW)/n0 estimator can come out <= 0 - both are real, not
#      edge-case-only outcomes when the true between-group signal is weak
#      or `group` is noisy relative to n.
estimate_sigma_G <- function(X, y, group, min_groups = 5) {
  group <- factor(group)
  n_groups <- nlevels(group)

  if (n_groups < min_groups) {
    warning(
      "estimate_sigma_G(): only ", n_groups, " groups (< min_groups = ", min_groups,
      ") - too few for a reliable between-group variance estimate. Returning NULL; ",
      "the caller should leave stochtree's default rfx variance prior untouched."
    )
    return(NULL)
  }

  n <- length(y)
  p <- ncol(X)
  dat <- cbind(y = y, X)
  dat$.group <- group

  sigma_G_est <- NA_real_

  if (requireNamespace("lme4", quietly = TRUE) && n > p + 1) {
    fml <- stats::as.formula(paste("y ~", paste(colnames(X), collapse = " + "), "+ (1 | .group)"))
    fit <- try(suppressMessages(suppressWarnings(lme4::lmer(fml, data = dat))), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      vc <- as.data.frame(lme4::VarCorr(fit))
      row <- vc[vc$grp == ".group" & is.na(vc$var2), , drop = FALSE]
      if (nrow(row) == 1) sigma_G_est <- row$sdcor
    }
  }

  if (!is.finite(sigma_G_est)) {
    # No-lme4 (or failed-fit) fallback: classical one-way random-effects
    # ANOVA variance-component estimator (Searle, Casella & McCulloch 1992,
    # sec. 3.3), computed on residuals from the fixed-effect-only fit -
    # avoids introducing a new, unjustified heuristic constant, at the cost
    # of being less efficient than REML (especially with unbalanced groups).
    p_fe <- p + n_groups - 1
    resid_e <- if (n > p_fe + 1) {
      stats::residuals(stats::lm(y ~ ., data = cbind(y = y, X)))
    } else {
      y - mean(y)
    }

    n_g <- as.numeric(table(group))
    grp_means <- tapply(resid_e, group, mean)
    grand_mean <- mean(resid_e)
    N <- length(resid_e)
    G <- n_groups

    ssb <- sum(n_g * (grp_means - grand_mean)^2)
    msb <- ssb / (G - 1)
    ssw <- sum((resid_e - grp_means[as.character(group)])^2)
    msw <- ssw / (N - G)
    n0 <- (N - sum(n_g^2) / N) / (G - 1)

    sigma_G2 <- (msb - msw) / n0
    sigma_G_est <- if (is.finite(sigma_G2) && sigma_G2 > 0) sqrt(sigma_G2) else NA_real_
  }

  if (!is.finite(sigma_G_est) || sigma_G_est <= 0) {
    warning(
      "estimate_sigma_G(): degenerate (zero/boundary/non-finite) between-group ",
      "variance estimate. Returning NULL; the caller should leave stochtree's ",
      "default rfx variance prior untouched."
    )
    return(NULL)
  }

  sigma_G_est
}

# hp_to_stochtree_rfx_variance_prior() mirrors hp_to_stochtree_sigma2_prior()
# exactly: same hp$sigdf/hp$sigquant quantile-matching recipe
# (P(sigma_G < sigma_G_est) = sigquant under a sigdf-df chi-square-calibrated
# inverse gamma), same `/ y_var` internal-scale conversion - just targeting
# sigma_G instead of the residual sigma. Returns NULL (propagated from
# estimate_sigma_G()) when calibration isn't trustworthy, rather than ever
# returning a fabricated prior.
#
# A note on `group_parameter_prior_cov` (the third element returned below):
# stochtree's own help page documents it as "Prior covariance matrix for the
# random effects 'group parameters'" (default NULL) - NOT as an initial
# value, unlike sigma2_global_init for the global-variance prior. Checked
# against stochtree's C++ source
# (src/random_effects.cpp, MultivariateRegressionRandomEffectsModel::
# SampleVarianceComponents()) to confirm what it actually does: the group
# parameter variance IS resampled every MCMC iteration via Gibbs, as
# IG(posterior_shape, posterior_scale) with the posterior built from
# variance_prior_shape_/variance_prior_scale_ (the a_xi/b_xi hyperparameters
# above) and the current group parameters - so group_parameter_prior_cov's
# user-supplied value only ever seeds the *first* iteration's covariance;
# every iteration after that overwrites it. Functionally it behaves exactly
# like sigma2_global_init (a starting value, not a fixed-for-the-chain
# prior), even though stochtree's docs don't call it that - both are set
# here from the same point estimate for consistency with that behaviour.
#
# `working_parameter_prior_cov` (sigma2_alpha) is deliberately left
# untouched at stochtree's own default (see header comment: this is the
# precondition for the shape/scale calibration below to correspond to the
# marginal variance of the random intercept at all).
hp_to_stochtree_rfx_variance_prior <- function(hp, X, y, group) {
  sigma_G_est <- estimate_sigma_G(X, y, group)
  if (is.null(sigma_G_est)) return(NULL)

  y_var <- stats::var(y)
  lambda <- sigma_G_est^2 * stats::qchisq(1 - hp$sigquant, df = hp$sigdf) / hp$sigdf
  lambda_std <- lambda / y_var

  list(
    variance_prior_shape = hp$sigdf / 2,
    variance_prior_scale = hp$sigdf * lambda_std / 2,
    group_parameter_prior_cov = sigma_G_est^2 / y_var
  )
}

# hp_to_stochtree_variance_priors() is the single entry point: one call
# returns calibrated hyperparameters for BOTH the global error-variance prior
# and the random-intercept variance prior, each already named and grouped to
# match stochtree's own argument lists exactly, per
# https://stochtree.ai/about/bart.html:
#   - $general_params:        sigma2_global_shape, sigma2_global_scale,
#                              sigma2_global_init
#                              (not in a table on that page - these names
#                              come from stochtree::bart()'s own source, and
#                              are the same ones hp_to_stochtree_sigma2_prior()
#                              has used throughout this package's benchmark
#                              code)
#   - $random_effects_params: variance_prior_shape, variance_prior_scale,
#                              group_parameter_prior_cov
#                              (= a_xi, b_xi, sigma2_xi in the page's
#                              "Random Effects Prior Terms Mapping" table)
#
# Each is ready to splice directly into the corresponding stochtree::bart()/
# bcf() argument, e.g.:
#   out <- hp_to_stochtree_variance_priors(hp, X, y, group)
#   stochtree::bart(..., general_params = out$general_params,
#                    random_effects_params = c(list(model_spec = "intercept_only"),
#                                               out$random_effects_params))
#
# `group` is optional: when omitted (no random-effects model), only
# $general_params is populated and $random_effects_params is NULL. When
# `group` is supplied but estimate_sigma_G() judges the between-group
# variance estimate degenerate, $random_effects_params is NULL too (the
# "break loudly, return NULL" behaviour propagates up from
# hp_to_stochtree_rfx_variance_prior() unchanged) - callers splicing a NULL
# element via c() simply contribute nothing, leaving stochtree's own default
# rfx variance prior in place.
hp_to_stochtree_variance_priors <- function(hp, X, y, group = NULL) {
  list(
    general_params = hp_to_stochtree_sigma2_prior(hp, X, y, group = group),
    random_effects_params = if (is.null(group)) NULL else hp_to_stochtree_rfx_variance_prior(hp, X, y, group)
  )
}

# ============================ 3. Demo / sanity checks ========================
#
# DGP: Friedman's function (the field-standard BART benchmark) plus an
# explicit random intercept per group, with known group_effect/sd_group -
# adapted from examples/benchmark/R/dgp.R's simulate_friedman_rfx(), copied
# here so this script is self-contained.

friedman_f <- function(X) {
  10 * sin(pi * X$x1 * X$x2) + 20 * (X$x3 - 0.5)^2 + 10 * X$x4 + 5 * X$x5
}

simulate_friedman_rfx <- function(n, n_groups = 20, sd_group = 1, y_sd = 1, n_noise_vars = 5) {
  p <- 5 + n_noise_vars
  X <- as.data.frame(matrix(stats::runif(n * p), ncol = p))
  colnames(X) <- paste0("x", seq_len(p))

  f_true <- friedman_f(X)
  group <- factor(sample.int(n_groups, n, replace = TRUE))
  group_effect <- stats::rnorm(n_groups, sd = sd_group)
  mu_true <- f_true + group_effect[as.integer(group)]
  y <- stats::rnorm(n, mean = mu_true, sd = y_sd)

  list(
    data = cbind(data.frame(y = y, group = group), X),
    f_true = f_true, group_effect = group_effect, mu_true = mu_true
  )
}

hp <- list(sigdf = 3, sigquant = 0.9)

# --- 3a. Does estimate_sigma_G() recover a known sd_group? ------------------
cat("\n=== estimate_sigma_G() recovery check (20 groups, n = 300) ===\n")
set.seed(1)
for (sd_group_true in c(0.5, 2, 5)) {
  sim <- simulate_friedman_rfx(n = 300, n_groups = 20, sd_group = sd_group_true, y_sd = 1)
  X <- sim$data[, grep("^x", colnames(sim$data))]
  est <- estimate_sigma_G(X, sim$data$y, sim$data$group)
  cat(sprintf("true sd_group = %5.2f   estimated sigma_G = %5.2f\n", sd_group_true, est))
}

# --- 3b. Degenerate case: too few groups triggers a loud warning + NULL -----
cat("\n=== Degenerate case: 3 groups (< min_groups = 5) ===\n")
set.seed(2)
sim_small <- simulate_friedman_rfx(n = 60, n_groups = 3, sd_group = 2, y_sd = 1)
X_small <- sim_small$data[, grep("^x", colnames(sim_small$data))]
est_small <- withCallingHandlers(
  estimate_sigma_G(X_small, sim_small$data$y, sim_small$data$group),
  warning = function(w) { cat("  [warning caught] ", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
)
cat("  result:", if (is.null(est_small)) "NULL (as expected)" else est_small, "\n")

# --- 3c. End-to-end: calibrated vs default rfx prior in stochtree::bart -----
cat("\n=== Calibrated vs default rfx prior: recovering group_effect ===\n")
set.seed(3)
sim <- simulate_friedman_rfx(n = 300, n_groups = 20, sd_group = 3, y_sd = 1)
X <- sim$data[, grep("^x", colnames(sim$data))]
y <- sim$data$y
group <- sim$data$group

calibrated <- hp_to_stochtree_variance_priors(hp, X, y, group)
cat("Calibrated general_params (global sigma2):\n")
str(calibrated$general_params)
cat("Calibrated random_effects_params (rfx intercept variance):\n")
str(calibrated$random_effects_params)

fit_default <- stochtree::bart(
  X_train = X, y_train = y, rfx_group_ids_train = as.integer(group),
  num_gfr = 0, num_burnin = 50, num_mcmc = 100,
  random_effects_params = list(model_spec = "intercept_only")
)

fit_calibrated <- stochtree::bart(
  X_train = X, y_train = y, rfx_group_ids_train = as.integer(group),
  num_gfr = 0, num_burnin = 50, num_mcmc = 100,
  general_params = calibrated$general_params,
  random_effects_params = c(list(model_spec = "intercept_only"), calibrated$random_effects_params)
)

# Posterior-mean group effect (one value per group, in the original factor
# level order) for each fit, compared to the true group_effect used to
# generate the data.
group_effect_rmse <- function(fit) {
  basis <- matrix(1, nrow = nlevels(group), ncol = 1)
  preds <- fit$rfx_samples$predict(seq_len(nlevels(group)), basis)
  est_group_effect <- rowMeans(preds)
  sqrt(mean((est_group_effect - sim$group_effect)^2))
}

cat(sprintf(
  "RMSE of posterior-mean group effect vs truth - default prior: %.3f, calibrated prior: %.3f\n",
  group_effect_rmse(fit_default), group_effect_rmse(fit_calibrated)
))

# --- 3d. hp_to_stochtree_variance_priors() without a random-effects model ---
# `group` is optional - omitting it calibrates only the global-variance
# prior, leaving $random_effects_params NULL rather than erroring.
cat("\n=== hp_to_stochtree_variance_priors() with no `group` (no rfx model) ===\n")
no_rfx <- hp_to_stochtree_variance_priors(hp, X, y)
str(no_rfx)
