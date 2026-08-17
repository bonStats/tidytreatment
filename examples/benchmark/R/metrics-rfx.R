# Random-effects prediction extraction + metrics. Prediction accuracy reuses
# Part A's generic metrics (rmse/coverage/crps_from_draws on a `fit` column,
# exactly as in metrics.R) against mu_true = f(x) + group_effect[group] - the
# sharper, rfx-specific diagnostic is whether each engine's own *group*
# effect estimates recover the true per-group intercepts.

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

# BART's own trees can absorb some of the overall/baseline level that would
# otherwise go into the random intercepts (the two components aren't
# separately identified from data alone), so raw bias in the group-effect
# estimates isn't meaningful on its own - centre both before comparing
# (removes any shared shift) and report correlation alongside as a
# shift-invariant recovery check.
group_effect_recovery <- function(estimated, true_group_effect) {
  dplyr::tibble(
    group_effect_cor = stats::cor(estimated, true_group_effect),
    group_effect_centered_rmse = rmse(
      estimated - mean(estimated),
      true_group_effect - mean(true_group_effect)
    )
  )
}
