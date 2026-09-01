# Random-effects prediction extraction + metrics. Prediction accuracy reuses
# Part A's generic metrics (metrics.R) against mu_true = f(x) +
# group_effect[group]; the rfx-specific diagnostic is whether each engine's
# own *group* effect estimates recover the true per-group intercepts.

extract_group_effects_stan4bart <- function(fit, group_col = ".group") {
  re <- dbarts::extract(fit, type = "ranef", combine_chains = FALSE)[[group_col]]
  # dims [1, n_groups, n_draws, n_chains] - average over everything but the
  # group dimension (dim 2).
  as.vector(apply(re, 2, mean))
}

extract_group_effects_stochtree <- function(fit, group_ids_unique) {
  rfx_basis <- matrix(1, nrow = length(group_ids_unique), ncol = 1)
  pred <- fit$rfx_samples$predict(group_ids_unique, rfx_basis) * fit$model_params$outcome_scale
  rowMeans(pred)
}

# BART's trees can absorb some of the baseline level that would otherwise go
# into the random intercepts (not separately identified from data alone), so
# raw bias isn't meaningful - centre both before comparing, plus correlation
# as a shift-invariant check.
group_effect_recovery <- function(estimated, true_group_effect) {
  dplyr::tibble(
    group_effect_cor = stats::cor(estimated, true_group_effect),
    group_effect_centered_rmse = rmse(
      estimated - mean(estimated),
      true_group_effect - mean(true_group_effect)
    ),
    # Uncentred: also picks up the non-identified shift between trees and
    # random intercepts (see the header comment above), so a gap between
    # this and the centred RMSE is expected and not itself a sign of poor
    # recovery - kept for reference alongside the centred metric, not as a
    # replacement for it.
    group_effect_raw_rmse = rmse(estimated, true_group_effect)
  )
}

# MCMC mixing diagnostics (effective sample size), on the raw sampled draws -
# not on any posterior mean. This is a check on the *sampler*, not the prior:
# two engines can have equally sensible priors and equally unbiased posterior
# means, yet one can still produce a noisier posterior mean at the same
# nominal draw count if its chain is more autocorrelated. Investigated
# because stan4bart's baseline consistently outperformed stochtree::bart's on
# prediction RMSE for the continuous rfx case, at a gap that did not shrink
# between n=250 and n=1000 - ruled out the tree and random-effects priors
# first (both engines recover sigma_G and Friedman's function equally well on
# average), then found stochtree's ESS for sigma^2 running 6-10x lower than
# stan4bart's at matched draw counts, which is a sampler-mixing effect (not a
# function of n) and matches a variance/bias decomposition showing both
# engines equally unbiased but stochtree noisier.
#
# extract_sigma_global_draws(): global residual SD sigma, continuous outcome
# only - fixed at 1 for both engines under a probit link for binary, so
# there's nothing to check there (confirmed empirically: neither engine even
# exposes the parameter for a binary fit).
#
# Returns the SD (sigma), not the variance (sigma^2), for both engines -
# despite the name stan4bart uses ("aux") already being on the SD scale
# (confirmed earlier: prior_aux is a prior on sigma, not sigma^2), while
# stochtree's own sigma2_global_samples is genuinely the variance (its
# IG(shape, scale) prior targets sigma^2 directly). Returning these as-is
# was a real bug: every downstream comparison (ESS, ACF, the pairs plots)
# was silently comparing stan4bart's SD against stochtree's variance, not
# the same quantity - confirmed empirically (stan4bart's raw draws and
# sqrt(stochtree's draws) track closely at true sigma = 1; stochtree's raw,
# un-square-rooted draws sit noticeably higher). sqrt() here puts both
# engines on the same scale, and matches sigma_G below (already the SD).
extract_sigma_global_draws <- function(fit, engine, outcome) {
  if (outcome != "continuous") return(NULL)
  if (engine == "stan4bart") {
    pn <- dimnames(fit$stan)[[1]]
    if (!("aux.1" %in% pn)) return(NULL)
    as.numeric(fit$stan["aux.1", , ])
  } else {
    sqrt(fit$sigma2_global_samples)
  }
}

# Autocorrelation of the sigma^2 chain itself (continuous outcome only, same
# restriction as extract_sigma_global_draws() above) - lets the mixing
# comparison in the results show *why* ESS differs between engines, not just
# that it does. NULL wherever extract_sigma_global_draws() would be (binary
# outcome, or too few draws to make an ACF meaningful).
sigma_global_acf <- function(fit, engine, outcome, lag_max = 150) {
  draws <- extract_sigma_global_draws(fit, engine, outcome)
  if (is.null(draws) || length(draws) < 10) return(NULL)
  lag_max <- min(lag_max, length(draws) - 1)
  acf_vals <- as.numeric(stats::acf(draws, lag.max = lag_max, plot = FALSE)$acf)
  dplyr::tibble(lag = 0:lag_max, acf = acf_vals)
}

# extract_sigma_group_draws(): random-intercept variance sigma_G, sampled for
# both outcomes (unlike sigma^2 above) - the natural substitute for the
# binary case. stan4bart exposes it directly as "theta_L.1". stochtree has no
# equivalent single parameter exposed the same way: sigma_samples/xi_samples
# from extract_parameter_samples() are on stochtree's internal non-centered
# reparameterization, not the effective group-effect scale (confirmed
# empirically - naively converting them gave nonsense, off by a large and
# inconsistent factor from the true sigma_G). The reliable route is the
# per-draw across-group SD of beta_samples (the resampled, final group
# intercepts - the same quantity extract_group_effects_stochtree() itself
# is built on), converted to the raw/latent scale via outcome_scale (1 for
# a probit fit, since the latent has no free scale to estimate).
extract_sigma_group_draws <- function(fit, engine) {
  if (engine == "stan4bart") {
    pn <- dimnames(fit$stan)[[1]]
    if (!("theta_L.1" %in% pn)) return(NULL)
    as.numeric(fit$stan["theta_L.1", , ])
  } else {
    rfx_params <- fit$rfx_samples$extract_parameter_samples()
    beta_raw <- rfx_params$beta_samples * fit$model_params$outcome_scale
    apply(beta_raw, 2, stats::sd)
  }
}

# ess_safe() now lives in metrics.R (shared with the vanilla prediction
# benchmark's own fitted-value ESS check) - metrics.R is sourced before this
# file in benchmark-prediction-rfx.qmd's setup chunk.

# Follow-up to the sigma^2 ACF investigation above: both engines update their
# non-tree parameters conditional on the *current* tree ensemble (confirmed
# by reading both packages' own sampling loops - this is blocked-Gibbs on
# both sides, not something HMC escapes), so the shared bottleneck candidate
# is how quickly the trees' own fit moves between sweeps. That isn't
# observable from sigma^2 alone. This tracks the fitted-value *distribution*
# per draw instead: for each retained draw, five quantiles (0.05, 0.2, 0.5,
# 0.8, 0.95) of the fitted values across observations, giving five per-draw
# time series whose own ACF can be compared to sigma^2's - reuses the same
# `draws` tibble already extracted for RMSE/coverage/CRPS, no extra fitting.
# `.draw` is confirmed (empirically, both engines) to run 1:N in the same
# order as the raw sigma^2/sigma_G draws, which is what makes the pairwise
# joint values below meaningful.
fitted_quantile_draws <- function(draws, value_col = "fit", probs = c(0.05, 0.2, 0.5, 0.8, 0.95)) {
  wide <- draws %>%
    dplyr::select(".row", ".draw", dplyr::all_of(value_col)) %>%
    tidyr::pivot_wider(names_from = ".draw", values_from = dplyr::all_of(value_col)) %>%
    dplyr::arrange(.data$.row)
  mat <- as.matrix(wide[, -1, drop = FALSE])
  draw_ids <- as.integer(colnames(mat))
  q <- apply(mat, 2, stats::quantile, probs = probs)
  rownames(q) <- paste0("q", probs * 100)
  out <- as.data.frame(t(q))
  out$.draw <- draw_ids
  out[order(out$.draw), , drop = FALSE]
}

fitted_quantile_acf <- function(draws, value_col = "fit", probs = c(0.05, 0.2, 0.5, 0.8, 0.95), lag_max = 150) {
  q_df <- fitted_quantile_draws(draws, value_col, probs)
  qcols <- setdiff(names(q_df), ".draw")
  dplyr::bind_rows(lapply(qcols, function(qname) {
    x <- q_df[[qname]]
    if (length(x) < 10) return(NULL)
    lm <- min(lag_max, length(x) - 1)
    dplyr::tibble(quantile = qname, lag = 0:lm, acf = as.numeric(stats::acf(x, lag.max = lm, plot = FALSE)$acf))
  }))
}
