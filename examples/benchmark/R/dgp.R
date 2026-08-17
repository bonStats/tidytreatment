# Data-generating processes for the benchmark, plus ground-truth extraction.

# ---- DGP 1: Friedman's function (Part A only) ---------------------------
# The field-standard BART benchmark (Friedman 1991; Chipman, George & McCulloch
# 2010; used again in Hill 2011 and Hahn et al. 2020). 5 relevant covariates,
# padded with `n_noise_vars` irrelevant ones (default 5, giving p = 10, the
# usual literature convention).

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

# Binary variant: true probability is a probit transform of the standardized
# f(x), so the average predicted probability sits near 0.5 with a plausible
# spread - this is a definitional choice (there's no "canonical" binary
# Friedman DGP in the literature), documented here so it's not mistaken for
# an established benchmark.
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
# simulate_su_hill_data()'s own n_subjects/sd_subjects mechanism adds a
# subject random effect directly to noisy y (not to mean_y), and doesn't
# return the true per-subject effects at all - no ground truth is
# recoverable from it for this purpose. This DGP is self-contained instead:
# same Friedman f(x) as simulate_friedman(), plus an explicit random
# intercept per group, with both the fixed-effect surface and the true group
# effects returned directly.

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

# Probit latent scale: f_std already has sd = 1 by construction, so
# sd_group's default is smaller than the continuous variant's (0.5, not 1) -
# a group effect of comparable *scale* to the fixed-effect surface, not
# comparable raw magnitude, given the different scales the two live on.
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
# The continuous-scale ground truth (mu0/mu1/ite/ate) now lives in the
# package itself: simulate_su_hill_data() computes mu0/mu1 while it still has
# the model matrix/coefficients/tau in scope and stashes them as a
# "ground_truth" attribute on the object it returns, and
# tidytreatment::su_hill_true_effects() is a thin accessor for that
# attribute - it directly completes the already-exported
# simulate_su_hill_data(), so it's general package functionality, not
# something specific to this benchmark. See R/simulate-su-hill.R in the
# package source for the implementation and
# tests/testthat/test-simulate-su-hill.R for the exact bit-for-bit
# verification against simulate_su_hill_data()'s own mean_y.

# Ground truth for the binary-outcome causal DGP: y_bin = 1(y_cont > threshold)
# where y_cont = mu(x,z) + N(0, y_sd^2) (simulate_su_hill_data()'s own
# generative model). The treatment effect on y_cont's mean (mu1 - mu0, what
# su_hill_true_effects() returns) is *not* the treatment effect on
# P(Y_bin = 1 | X) after thresholding - that requires the probit transform
# below. y_sd must match whatever was passed to simulate_su_hill_data()
# (default 1); threshold is typically median(y_cont) from the same simulated
# draw, so the same threshold used to build y_bin must be passed back in here
# too. This binarization is a benchmark-specific choice - simulate_su_hill_data()
# has no such feature itself - so it stays here rather than moving to the
# package.
su_hill_truth_binary <- function(sim, y_sd, threshold) {
  truth <- su_hill_true_effects(sim)
  p0 <- stats::pnorm((truth$mu0 - threshold) / y_sd)
  p1 <- stats::pnorm((truth$mu1 - threshold) / y_sd)
  list(p0 = p0, p1 = p1, ite = p1 - p0, ate = mean(p1 - p0))
}
