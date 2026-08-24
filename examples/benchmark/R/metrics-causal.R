# Part B metrics. Builds on the generic per-row metrics in metrics.R (rmse,
# coverage, posterior_mean_by_row, cross_engine_agreement all work directly
# on a `cte` value column, same as Part A's `fit`) and adds the draw-level
# ATE/ATT summaries and PEHE specific to treatment-effect draws.

# One ATE estimate per posterior draw (mean cte across units, within draw) -
# used for both the point estimate and its credible interval (quantiles over draws).
draw_level_ate <- function(te_draws, rows = NULL) {
  d <- if (is.null(rows)) te_draws else dplyr::filter(te_draws, .data$.row %in% rows)
  d %>%
    dplyr::group_by(.data$.draw) %>%
    dplyr::summarise(.ate = mean(.data$cte), .groups = "drop") %>%
    dplyr::pull(".ate")
}

ate_summary <- function(te_draws, true_ate, level = 0.95) {
  ate_draws <- draw_level_ate(te_draws)
  alpha <- 1 - level
  ci <- stats::quantile(ate_draws, c(alpha / 2, 1 - alpha / 2))
  dplyr::tibble(
    ate_est = mean(ate_draws),
    ate_bias = mean(ate_draws) - true_ate,
    ate_covered = true_ate >= ci[1] && true_ate <= ci[2]
  )
}

att_summary <- function(te_draws, z, true_ite, level = 0.95) {
  treated_rows <- which(z == 1)
  true_att <- mean(true_ite[treated_rows])
  ate_summary(dplyr::filter(te_draws, .data$.row %in% treated_rows), true_att, level) %>%
    dplyr::rename(att_est = "ate_est", att_bias = "ate_bias", att_covered = "ate_covered")
}

# PEHE (Hill 2011): rmse() of the per-unit posterior-mean cte against the
# true ITE. Meaningful only when true_ite varies (response_parallel = FALSE)
# - callers should gate on stats::sd(true_ite) > 0.
pehe <- function(te_draws, true_ite) {
  pm <- posterior_mean_by_row(te_draws, "cte")
  rmse(pm, true_ite)
}

# Agreement rate (fraction of units where both reach the same
# supported/not-supported decision) between bartCause's own native SD-rule
# common-support flag (the `supported` column from treatment_effects.bartcFit())
# and tidytreatment's independently-coded has_common_support(), both applied
# to bartc_fit$fit.rsp - a genuine two-implementations-of-the-same-rule
# check, not a substitute-model comparison. Only meaningful for bartc -
# stochtree has no native common-support mechanism to compare against.
bartc_common_support_agreement <- function(bartc_fit, treatment_col = ".z", method = "sd") {
  te_native <- tidytreatment::treatment_effects(bartc_fit, common_support_method = method)
  native_supported <- te_native %>%
    dplyr::distinct(.data$.row, .data$supported) %>%
    dplyr::arrange(.data$.row) %>%
    dplyr::pull("supported")

  # fit.rsp has no $terms component, so has_common_support()'s default
  # model.matrix() extraction can't be used; @x is a plain numeric matrix,
  # so the treatment column needs coercing back to integer.
  modeldata <- as.data.frame(bartc_fit$data.rsp@x)
  modeldata[[treatment_col]] <- as.integer(modeldata[[treatment_col]])

  cs_own <- tidytreatment::has_common_support(bartc_fit$fit.rsp, treatment = treatment_col, method = method, modeldata = modeldata)
  own_supported <- cs_own$common_support

  stopifnot(length(native_supported) == length(own_supported))
  mean(native_supported == own_supported, na.rm = TRUE)
}
