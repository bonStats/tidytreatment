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

# One replication's worth of work: simulate one dataset, fit every row
# (engine/variant) against it, and return this cell's contribution to each
# accumulator as a single named list - the unit of parallelism (see
# R/parallel-driver.R). No per-row seed is needed here (unlike rfx's
# fit_seed): fit-engines-prediction.R's fit_*() functions take no seed
# argument at all, relying entirely on the ambient RNG stream established by
# `set.seed(rep_seed)` below, consumed sequentially across the row loop -
# which is exactly why parallelism here is at the `rep` grain, keeping that
# whole sequential row loop inside one worker.
run_prediction_cell <- function(outcome, n, rep, B, rows, hp, seed, y_sd, n_for_examples) {
  metrics <- list()
  examples <- list()

  rep_seed <- seed * 1e6 + n * 1e3 + rep
  set.seed(rep_seed)

  dgp <- if (outcome == "continuous") simulate_friedman(n, y_sd = y_sd) else simulate_friedman_binary(n)
  X <- dplyr::select(dgp$data, dplyr::starts_with("x"))
  y <- dgp$data$y
  truth <- if (outcome == "continuous") dgp$f_true else dgp$prob_true

  fitted_means <- list()
  keep_example <- (n == n_for_examples) && (rep == 1)

  for (row in rows) {
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
      fit_time_sec = fit_time_sec,
      fitted_ess = fitted_value_ess(draws, "fit")
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

  list(
    metrics = dplyr::bind_rows(metrics),
    agreement = agr,
    examples = dplyr::bind_rows(examples)
  )
}

# Attached/sourced by every worker before its first cell - mirrors
# benchmark-prediction.qmd's own setup chunk. See
# R/parallel-driver.R:with_worker_setup() for why this runs unconditionally
# on every call rather than being gated to run once per worker.
prediction_worker_setup <- function(benchmark_dir) {
  library(dplyr); library(tidyr); library(tidytreatment); library(tidybayes)
  library(BART); library(dbarts); library(stochtree)
  for (f in c("cache.R", "hyperparams.R", "dgp.R", "metrics.R",
              "fit-engines-prediction.R", "run-benchmark-prediction.R")) {
    source(file.path(benchmark_dir, "R", f))
  }
}

# epred_draws() is called with no explicit `scale`: every engine's default
# resolves to the response scale on its own (continuous -> "linear"/no-op,
# binary -> "probability"), so no outcome-specific branching is needed here.
run_benchmark_prediction <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L, y_sd = 1,
                                      benchmark_dir = getwd()) {
  n_for_examples <- max(n_values)
  outcomes <- c("continuous", "binary")
  grid <- build_benchmark_grid(outcomes, n_values, B)

  cell_fn <- with_worker_setup(
    setup_fn = function() prediction_worker_setup(benchmark_dir),
    cell_fn = function(cell) {
      rows <- prediction_rows(cell$outcome)
      run_prediction_cell(cell$outcome, cell$n, cell$rep, B, rows, hp, seed, y_sd, n_for_examples)
    }
  )
  label_fn <- function(cell) paste("outcome =", cell$outcome, "n =", cell$n, "rep =", paste0(cell$rep, "/", B))

  results <- run_cells_parallel(grid, cell_fn, label_fn)

  list(
    metrics = dplyr::bind_rows(lapply(results, `[[`, "metrics")),
    agreement = dplyr::bind_rows(lapply(results, `[[`, "agreement")),
    examples = dplyr::bind_rows(lapply(results, `[[`, "examples"))
  )
}
