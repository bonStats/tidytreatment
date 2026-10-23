# Vanilla prediction MCMC mixing diagnostics - the counterpart of
# metrics-rfx.R's extract_sigma_global_draws()/sigma_global_acf(), ported
# here after benchmark-prediction-rfx.qmd's own investigation into a
# persistent stan4bart-vs-stochtree RMSE gap turned out to be a
# sampler-mixing issue rather than a modelling one. Continuous outcome only:
# sigma is fixed at 1 under every engine's probit link for a binary outcome,
# same restriction as the rfx benchmark.

# Returns sigma (the SD, not the variance) for each engine's own raw fit
# object, trimmed to the last `n_draws` values when given.
#
# stochtree::bart's `$sigma2_global_samples` is the *variance*, not the SD -
# the exact same distinction already found and fixed for
# extract_sigma_global_draws() in metrics-rfx.R (comparing stan4bart's raw
# SD draws against stochtree's raw, un-square-rooted draws was a real bug
# there). Applying sqrt() here from the start avoids reintroducing it.
#
# `n_draws` (typically dplyr::n_distinct(draws$.draw) from the epred_draws()
# output already extracted for the same fit) matters more than it should:
# BART::wbart's own help page claims "$sigma: post burn in draws of sigma,
# length = ndpost", with burn-in living separately in `$first.sigma` - but
# empirically (confirmed directly, nskip=20/ndpost=30) `$sigma` comes back
# length 50 (nskip+ndpost) and `$first.sigma` is empty, i.e. burn-in is
# included in `$sigma` after all, contradicting the docs. dbarts::bart2's
# own `$sigma` and stochtree's `$sigma2_global_samples` do NOT have this
# problem (already exactly `n_draws` long) - trimming to the *last*
# `n_draws` values is a no-op for them and the correct fix for wbart, so
# it's applied uniformly rather than special-cased per engine.
extract_sigma_draws_prediction <- function(fit, engine, n_draws = NULL) {
  draws <- switch(engine,
    "BART::wbart" = as.numeric(fit$sigma),
    "dbarts::bart2" = as.numeric(fit$sigma),
    "stochtree::bart" = sqrt(fit$sigma2_global_samples),
    stop("extract_sigma_draws_prediction(): unknown engine '", engine, "'")
  )
  if (!is.null(n_draws) && length(draws) > n_draws) draws <- utils::tail(draws, n_draws)
  draws
}

# Autocorrelation of the sigma chain - same shape as metrics-rfx.R's
# sigma_global_acf(), minus the outcome-type branching (callers only invoke
# this for continuous rows).
sigma_acf_prediction <- function(fit, engine, n_draws = NULL, lag_max = 150) {
  draws <- extract_sigma_draws_prediction(fit, engine, n_draws = n_draws)
  if (length(draws) < 10) return(NULL)
  lag_max <- min(lag_max, length(draws) - 1)
  acf_vals <- as.numeric(stats::acf(draws, lag.max = lag_max, plot = FALSE)$acf)
  dplyr::tibble(lag = 0:lag_max, acf = acf_vals)
}
