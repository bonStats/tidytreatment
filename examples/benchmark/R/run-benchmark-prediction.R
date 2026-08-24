# Part A orchestration: loops DGP (continuous/binary) x n x replication x
# engine/variant row, fitting through fit-engines-prediction.R and extracting
# through tidytreatment's own epred_draws(), not the raw model object - the
# tidytreatment wrapper itself is what's being benchmarked here.

# "default" rows use each package's own out-of-the-box hyperprior
# settings instead of baseline_hyperparams(), so the summary table shows how
# much of baseline's accuracy comes from the prior choices themselves. No
# such row exists for BART::wbart: its own defaults already equal baseline
# (see hyperparams.R's "Package-default hyperprior variants" section) - the
# Hyperparameters section notes this rather than duplicating the fit.
prediction_rows <- function(outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  if (outcome == "continuous") {
    list(
      list(engine = "BART::wbart", variant = "baseline",
           fit = function(X, y, hp) fit_wbart(X, y, hp)),
      list(engine = "dbarts::bart2", variant = "baseline",
           fit = function(X, y, hp) fit_dbarts_bart(X, y, hp, outcome = "continuous")),
      list(engine = "dbarts::bart2", variant = "default",
           fit = function(X, y, hp) fit_dbarts_bart(X, y, hp, outcome = "continuous", variant = "default")),
      list(engine = "stochtree::bart", variant = "baseline",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 0)),
      list(engine = "stochtree::bart", variant = "baseline+gfr",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 5)),
      list(engine = "stochtree::bart", variant = "baseline+leaf_var",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 0, sample_leaf_var = TRUE)),
      list(engine = "stochtree::bart", variant = "default",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "continuous", variant = "default"))
    )
  } else {
    list(
      list(engine = "BART::pbart", variant = "baseline",
           fit = function(X, y, hp) fit_pbart(X, y, hp)),
      list(engine = "BART::pbart", variant = "default",
           fit = function(X, y, hp) fit_pbart(X, y, hp, variant = "default")),
      list(engine = "dbarts::bart2", variant = "baseline",
           fit = function(X, y, hp) fit_dbarts_bart(X, y, hp, outcome = "binary")),
      list(engine = "dbarts::bart2", variant = "default",
           fit = function(X, y, hp) fit_dbarts_bart(X, y, hp, outcome = "binary", variant = "default")),
      list(engine = "stochtree::bart", variant = "baseline",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 0)),
      list(engine = "stochtree::bart", variant = "baseline+gfr",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 5)),
      list(engine = "stochtree::bart", variant = "baseline+leaf_var",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 0, sample_leaf_var = TRUE)),
      list(engine = "stochtree::bart", variant = "default",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "binary", variant = "default"))
    )
  }
}

# epred_draws() is called with no explicit `scale`: every engine's default
# resolves to the response scale on its own (continuous -> "linear"/no-op,
# binary -> "probability"), so no outcome-specific branching is needed here.
run_benchmark_prediction <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L) {
  metrics <- list()
  agreement <- list()
  examples <- list()
  n_for_examples <- max(n_values)

  # Continuous and binary no longer have the same row count now that
  # default rows exist per-outcome (e.g. BART::pbart's is binary-only,
  # since wbart has no default row at all) - sum each outcome separately
  # rather than assuming symmetry.
  n_rows <- length(prediction_rows("continuous")) + length(prediction_rows("binary"))
  total_fits <- length(n_values) * B * n_rows
  fit_i <- 0

  for (outcome in c("continuous", "binary")) {
    rows <- prediction_rows(outcome)

    for (n in n_values) {
      for (rep in seq_len(B)) {
        rep_seed <- seed * 1e6 + n * 1e3 + rep
        set.seed(rep_seed)

        dgp <- if (outcome == "continuous") simulate_friedman(n) else simulate_friedman_binary(n)
        X <- dplyr::select(dgp$data, dplyr::starts_with("x"))
        y <- dgp$data$y
        truth <- if (outcome == "continuous") dgp$f_true else dgp$prob_true

        fitted_means <- list()
        keep_example <- (n == n_for_examples) && (rep == 1)

        for (row in rows) {
          fit_i <- fit_i + 1
          progress_note(fit_i, total_fits, "outcome =", outcome, "n =", n, "rep =", paste0(rep, "/", B), "engine =", row$engine, row$variant)

          t0 <- Sys.time()
          fit <- row$fit(X, y, hp)
          fit_time_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

          draws <- tidybayes::epred_draws(fit, newdata = X, include_newdata = FALSE, value = "fit")
          pm <- posterior_mean_by_row(draws, "fit")
          # Cross-engine agreement is a comparison of matched-prior fits only -
          # "default" and "baseline+gfr" rows use different priors/warm-start
          # settings and would muddy the comparison, so only "baseline" rows
          # are collected here.
          if (identical(row$variant, "baseline")) {
            fitted_means[[row$engine]] <- pm
          }

          metrics[[length(metrics) + 1]] <- dplyr::tibble(
            outcome = outcome, n = n, rep = rep,
            engine = row$engine, variant = row$variant,
            rmse = rmse(pm, truth),
            mae = mae(pm, truth),
            coverage95 = coverage(draws, "fit", truth, level = 0.95),
            crps = crps_from_draws(draws, "fit", truth),
            fit_time_sec = fit_time_sec
          )

          if (keep_example) {
            examples[[length(examples) + 1]] <- dplyr::tibble(
              outcome = outcome, engine = row$engine, variant = row$variant,
              .row = seq_along(pm), truth = truth, fitted = pm
            )
          }
        }

        agr <- cross_engine_agreement(fitted_means)
        agr$outcome <- outcome
        agr$n <- n
        agr$rep <- rep
        agreement[[length(agreement) + 1]] <- agr
      }
    }
  }

  list(
    metrics = dplyr::bind_rows(metrics),
    agreement = dplyr::bind_rows(agreement),
    examples = dplyr::bind_rows(examples)
  )
}
