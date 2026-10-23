# Shared DIY propensity-score fitting, used by every "DIY" row in Part B
# (BART/stochtree::bart two-step causal rows, and the bartc/bcf baseline-DIY
# rows) - fit once here so DIY rows across different causal engines are
# comparable (only the causal engine itself varies).
#
# `BART::pbart` is the one propensity-estimation engine for every DIY row,
# regardless of which causal engine consumes the score - otherwise "DIY vs
# built-in" would conflate two things varying at once.
#
# Two recipes:
#  - "two_stage": mirrors the package vignettes' VS -> PS approach - fit an
#    outcome model (y ~ X), take variables at/above the median avg_inclusion
#    importance, then fit propensity on that subset only.
#  - "ps_all": propensity fit directly on every confounder in X, no
#    variable-selection step.

fit_diy_propensity <- function(X, y, z, hp, recipe = c("two_stage", "ps_all")) {
  recipe <- match.arg(recipe)

  if (recipe == "two_stage") {
    vs_args <- hp_to_wbart(hp)
    vs_fit <- do.call(BART::wbart, c(list(x.train = X, y.train = y, printevery = 10000L), vs_args))

    importance <- covariate_importance(vs_fit)
    threshold <- stats::quantile(importance$avg_inclusion, 0.5)
    selected_vars <- importance$variable[importance$avg_inclusion >= threshold]
    selected_vars <- intersect(colnames(X), selected_vars)
    if (length(selected_vars) == 0) selected_vars <- colnames(X) # safety net
  } else {
    selected_vars <- colnames(X)
  }

  X_sel <- X[, selected_vars, drop = FALSE]
  ps_args <- hp_to_pbart(hp)
  ps_fit <- do.call(BART::pbart, c(list(x.train = X_sel, y.train = as.integer(z), printevery = 10000L), ps_args))

  draws <- tidybayes::epred_draws(ps_fit, newdata = X_sel, include_newdata = FALSE, value = "prop")
  propensity <- posterior_mean_by_row(draws, "prop")

  list(propensity = propensity, selected_vars = selected_vars, ps_fit = ps_fit)
}
