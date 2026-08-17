# Shared metrics layer. Operates on tidy epred_draws()-style long output
# (columns .row, .draw, <value_col>) plus a ground-truth vector indexed by
# .row (1:n, in row order) - i.e. exactly what tidytreatment's own extraction
# functions return, so this never touches a backend's raw fit object.
#
# Binary outcomes deliberately reuse the same rmse()/mae()/coverage()/
# crps_from_draws() as continuous ones, called with the *true probability*
# (not observed 0/1 labels) as truth and posterior draws on the probability
# scale as the value column - since the synthetic DGPs here give us the true
# probability directly, that's a strictly more informative ground-truth
# comparison than the usual label-based Brier score/log-loss, which is what
# a real (non-simulated) benchmark would have to fall back on instead.

posterior_mean_by_row <- function(draws, value_col) {
  draws %>%
    dplyr::group_by(.data$.row) %>%
    dplyr::summarise(.mean = mean(.data[[value_col]]), .groups = "drop") %>%
    dplyr::arrange(.data$.row) %>%
    dplyr::pull(".mean")
}

rmse <- function(est, truth) sqrt(mean((est - truth)^2))
mae <- function(est, truth) mean(abs(est - truth))

# 95% (or `level`) posterior credible interval coverage of the truth, by row.
coverage <- function(draws, value_col, truth, level = 0.95) {
  alpha <- 1 - level
  stopifnot(length(truth) == dplyr::n_distinct(draws$.row))

  draws %>%
    dplyr::group_by(.data$.row) %>%
    dplyr::summarise(
      .lower = stats::quantile(.data[[value_col]], alpha / 2),
      .upper = stats::quantile(.data[[value_col]], 1 - alpha / 2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$.row) %>%
    dplyr::mutate(.truth = truth, .covered = .data$.truth >= .data$.lower & .data$.truth <= .data$.upper) %>%
    dplyr::pull(".covered") %>%
    mean()
}

# Mean CRPS (continuous ranked probability score) across rows - a single
# proper score combining accuracy and uncertainty (Gneiting & Raftery 2007);
# lower is better. Requires one row of draws per observation (a full
# posterior sample per row, not just the mean).
crps_from_draws <- function(draws, value_col, truth) {
  stopifnot(has_installed_package("scoringRules"))
  stopifnot(length(truth) == dplyr::n_distinct(draws$.row))

  wide <- draws %>%
    dplyr::select(".row", ".draw", dplyr::all_of(value_col)) %>%
    tidyr::pivot_wider(names_from = ".draw", values_from = dplyr::all_of(value_col)) %>%
    dplyr::arrange(.data$.row)

  mat <- as.matrix(wide[, -1, drop = FALSE])
  mean(scoringRules::crps_sample(y = truth, dat = mat))
}

has_installed_package <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# Pairwise agreement between engines' posterior-mean fitted values on the
# *same* simulated dataset - the main "does implementation X disagree with
# the pack" smell test. `fitted_by_engine` is a named list of numeric
# vectors (posterior_mean_by_row() output), all the same length and row
# order.
cross_engine_agreement <- function(fitted_by_engine) {
  engines <- names(fitted_by_engine)
  stopifnot(length(engines) >= 2)

  pairs <- utils::combn(engines, 2, simplify = FALSE)
  dplyr::bind_rows(lapply(pairs, function(pair) {
    a <- fitted_by_engine[[pair[1]]]
    b <- fitted_by_engine[[pair[2]]]
    dplyr::tibble(
      engine_a = pair[1], engine_b = pair[2],
      correlation = stats::cor(a, b),
      rmse = rmse(a, b)
    )
  }))
}

# Generic outlier flag: within each group (e.g. a DGP setting), z-score
# `metric_col` across rows (e.g. engine/variant) and flag anything beyond
# `z_thresh` SDs from the group mean - the "does one implementation stand out
# from the pack" check used in each document's flags section. A flag here
# means "worth a human look," not "definitely a bug": with only a handful of
# rows per group the z-score is noisy, and a real difference between engines
# (e.g. a deliberately weaker baseline) will legitimately flag too.
flag_outliers <- function(summary_df, metric_col, group_cols, z_thresh = 2) {
  summary_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::mutate(
      .z = as.vector(scale(.data[[metric_col]])),
      flagged = !is.na(.data$.z) & abs(.data$.z) > z_thresh
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$flagged) %>%
    dplyr::select(-"flagged")
}
