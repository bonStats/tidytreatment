# Part B metrics. Builds on the generic per-row metrics in metrics.R (rmse,
# coverage, posterior_mean_by_row, cross_engine_agreement all work directly
# on a `cte` value column exactly as they do on Part A's `fit` column) and
# adds the draw-level ATE/ATT summaries and PEHE that are specific to
# treatment-effect draws.

# One ATE estimate per posterior draw (mean cte across units, within draw) -
# used for both the point estimate (mean over draws) and its credible
# interval (quantiles over draws), rather than only ever looking at the
# per-unit posterior means.
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

# PEHE (precision in estimating heterogeneous effects; Hill 2011) is just
# rmse() of the per-unit posterior-mean cte against the true ITE - meaningful
# only when true_ite actually varies (response_parallel = FALSE); for the
# homogeneous DGP every unit's truth is the same scalar (the ATE) and PEHE
# degenerates to (a version of) the same information ate_summary() already
# reports, so callers should gate on stats::sd(true_ite) > 0 before treating
# this as a distinct diagnostic.
pehe <- function(te_draws, true_ite) {
  pm <- posterior_mean_by_row(te_draws, "cte")
  rmse(pm, true_ite)
}

# Agreement rate (fraction of units where both methods reach the same
# supported/not-supported decision) between bartCause's own native SD-rule
# common-support flag (already threaded through treatment_effects.bartcFit()
# as the `supported` column) and tidytreatment's independently-coded
# has_common_support() - applied to bartc_fit$fit.rsp itself (the same fitted
# response-stage model bartc's own commonSup.rule uses internally), now that
# tidytreatment has epred_draws.bart()/predicted_draws.bart() support for raw
# dbarts "bart"-class objects (added specifically to enable this: fit.rsp is
# such an object whenever bartc() is fit without a `parametric` argument,
# which is every row in this benchmark - there's no grouping structure in
# these DGPs). This is a genuine two-implementations-of-the-same-rule check
# on bartc's own fitted values, not a substitute-model comparison. Only
# meaningful for bartc - stochtree has no native common-support mechanism to
# compare against.
bartc_common_support_agreement <- function(bartc_fit, treatment_col = ".z", method = "sd") {
  te_native <- tidytreatment::treatment_effects(bartc_fit, common_support_method = method)
  native_supported <- te_native %>%
    dplyr::distinct(.data$.row, .data$supported) %>%
    dplyr::arrange(.data$.row) %>%
    dplyr::pull("supported")

  # fit.rsp's actual training matrix, including bartc's own auto-added
  # propensity-score covariate ("ps", when p.scoreAsCovariate = TRUE, the
  # default) - has_common_support()'s own model.matrix()-based default can't
  # be used here (fit.rsp has no $terms component), and @x is a plain numeric
  # matrix so the treatment column needs coercing back to integer.
  modeldata <- as.data.frame(bartc_fit$data.rsp@x)
  modeldata[[treatment_col]] <- as.integer(modeldata[[treatment_col]])

  cs_own <- tidytreatment::has_common_support(bartc_fit$fit.rsp, treatment = treatment_col, method = method, modeldata = modeldata)
  own_supported <- cs_own$common_support

  stopifnot(length(native_supported) == length(own_supported))
  mean(native_supported == own_supported, na.rm = TRUE)
}
