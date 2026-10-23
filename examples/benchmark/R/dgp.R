# Data-generating processes for the benchmark, plus ground-truth extraction.

# ---- DGP 1: Friedman's function (Part A only) ---------------------------
# The field-standard BART benchmark (Friedman 1991). 5 relevant covariates,
# padded with `n_noise_vars` irrelevant ones (default 5, giving p = 10).

friedman_f <- function(X) {
  10 * sin(pi * X$x1 * X$x2) + 20 * (X$x3 - 0.5)^2 + 10 * X$x4 + 5 * X$x5
}

simulate_friedman <- function(n, y_sd = 1, n_noise_vars = 5) {
  p <- 5 + n_noise_vars
  X <- as.data.frame(matrix(stats::runif(n * p), ncol = p))
  colnames(X) <- paste0("x", seq_len(p))

  f_true <- friedman_f(X)
  y <- stats::rnorm(n, mean = f_true, sd = y_sd)

  list(data = cbind(data.frame(y = y), X), f_true = f_true)
}

# Binary variant: probit transform of standardized f(x). Not a canonical
# literature benchmark - a definitional choice for this suite only.
simulate_friedman_binary <- function(n, n_noise_vars = 5) {
  p <- 5 + n_noise_vars
  X <- as.data.frame(matrix(stats::runif(n * p), ncol = p))
  colnames(X) <- paste0("x", seq_len(p))

  f <- friedman_f(X)
  f_std <- (f - mean(f)) / stats::sd(f)
  prob_true <- stats::pnorm(f_std)
  y <- stats::rbinom(n, 1, prob_true)

  list(data = cbind(data.frame(y = y), X), prob_true = prob_true)
}

# ---- DGP 1b: Friedman's function + an explicit random intercept (rfx prediction doc) ----
# simulate_su_hill_data()'s own n_subjects/sd_subjects mechanism adds the
# random effect to noisy y directly and returns no true per-subject effects,
# so no ground truth is recoverable from it. This DGP returns both the
# fixed-effect surface and the true group effects directly instead.

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

# f_std has sd = 1 by construction, so sd_group defaults smaller than the
# continuous variant's (0.5, not 1) to stay comparable in scale.
simulate_friedman_rfx_binary <- function(n, n_groups = 20, sd_group = 0.5, n_noise_vars = 5) {
  p <- 5 + n_noise_vars
  X <- as.data.frame(matrix(stats::runif(n * p), ncol = p))
  colnames(X) <- paste0("x", seq_len(p))

  f <- friedman_f(X)
  f_std <- (f - mean(f)) / stats::sd(f)
  group <- factor(sample.int(n_groups, n, replace = TRUE))
  group_effect <- stats::rnorm(n_groups, sd = sd_group)
  eta_true <- f_std + group_effect[as.integer(group)]
  prob_true <- stats::pnorm(eta_true)
  y <- stats::rbinom(n, 1, prob_true)

  list(
    data = cbind(data.frame(y = y, group = group), X),
    prob_true = prob_true, group_effect = group_effect, eta_true = eta_true
  )
}

# ---- DGP 2: simulate_su_hill_data() ground truth (Part A continuity + Part B) ----
# Continuous-scale ground truth (mu0/mu1/ite/ate) lives in the package itself:
# simulate_su_hill_data() stashes it as a "ground_truth" attribute, and
# tidytreatment::su_hill_true_effects() is a thin accessor for it. See
# R/simulate-su-hill.R and tests/testthat/test-simulate-su-hill.R.

# Ground truth for the binary-outcome causal DGP: y_bin = 1(y_cont > threshold)
# where y_cont = mu(x,z) + N(0, y_sd^2). mu1 - mu0 (su_hill_true_effects()'s
# own output) is the treatment effect on y_cont's mean, not on
# P(Y_bin = 1 | X) after thresholding - hence the probit transform below.
# y_sd/threshold must match what was passed to/derived from
# simulate_su_hill_data(). Binarization is benchmark-specific, not part of
# simulate_su_hill_data() itself.
su_hill_truth_binary <- function(sim, y_sd, threshold) {
  truth <- su_hill_true_effects(sim)
  p0 <- stats::pnorm((truth$mu0 - threshold) / y_sd)
  p1 <- stats::pnorm((truth$mu1 - threshold) / y_sd)
  list(p0 = p0, p1 = p1, ite = p1 - p0, ate = mean(p1 - p0))
}
