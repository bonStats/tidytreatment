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
# Every row's `fit` closure takes the same (X, y, hp, seed) signature so
# run_prediction_cell()'s loop can call them uniformly - but only the
# stochtree::bart rows actually forward `seed` on into the fit. BART::wbart/
# pbart and dbarts::bart2 all respect the ambient set.seed() already
# established by run_prediction_cell()'s own `set.seed(rep_seed)` (confirmed
# empirically), so they don't need it; stochtree::bart's internal RNG
# ignores set.seed() entirely (see fit_stochtree_bart()'s own header
# comment), so it's the one engine here that does.
prediction_rows <- function(outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  if (outcome == "continuous") {
    list(
      list(engine = "BART::wbart", variant = "baseline",
           fit = function(X, y, hp, seed) fit_wbart(X, y, hp)),
      list(engine = "dbarts::bart2", variant = "baseline",
           fit = function(X, y, hp, seed) fit_dbarts_bart(X, y, hp, outcome = "continuous")),
      list(engine = "dbarts::bart2", variant = "default",
           fit = function(X, y, hp, seed) fit_dbarts_bart(X, y, hp, outcome = "continuous", variant = "default")),
      list(engine = "stochtree::bart", variant = "baseline",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 0, seed = seed)),
      list(engine = "stochtree::bart", variant = "baseline+gfr",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 5, seed = seed)),
      list(engine = "stochtree::bart", variant = "baseline+leaf_var",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 0, sample_leaf_var = TRUE, seed = seed)),
      list(engine = "stochtree::bart", variant = "default",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "continuous", variant = "default", seed = seed))
    )
  } else {
    list(
      list(engine = "BART::pbart", variant = "baseline",
           fit = function(X, y, hp, seed) fit_pbart(X, y, hp)),
      list(engine = "BART::pbart", variant = "default",
           fit = function(X, y, hp, seed) fit_pbart(X, y, hp, variant = "default")),
      list(engine = "dbarts::bart2", variant = "baseline",
           fit = function(X, y, hp, seed) fit_dbarts_bart(X, y, hp, outcome = "binary")),
      list(engine = "dbarts::bart2", variant = "default",
           fit = function(X, y, hp, seed) fit_dbarts_bart(X, y, hp, outcome = "binary", variant = "default")),
      list(engine = "stochtree::bart", variant = "baseline",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 0, seed = seed)),
      list(engine = "stochtree::bart", variant = "baseline+gfr",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 5, seed = seed)),
      list(engine = "stochtree::bart", variant = "baseline+leaf_var",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 0, sample_leaf_var = TRUE, seed = seed)),
      list(engine = "stochtree::bart", variant = "default",
           fit = function(X, y, hp, seed) fit_stochtree_bart(X, y, hp, outcome = "binary", variant = "default", seed = seed))
    )
  }
}

# One replication's worth of work: simulate one dataset, fit every row
# (engine/variant) against it, and return this cell's contribution to each
# accumulator as a single named list - the unit of parallelism (see
# R/parallel-driver.R). Most engines here rely entirely on the ambient RNG
# stream established by `set.seed(rep_seed)` below, consumed sequentially
# across the row loop - which is exactly why parallelism here is at the
# `rep` grain, keeping that whole sequential row loop inside one worker.
# stochtree::bart is the exception (its internal RNG ignores set.seed()
# entirely - see fit_stochtree_bart()'s header comment), so it gets an
# explicit, deterministic `fit_seed` - a pure function of cell identity, the
# same fit_seed formula already used in run-benchmark-rfx.R's run_rfx_cell(),
# so it stays safe under out-of-order or concurrent execution.
run_prediction_cell <- function(outcome, outcome_code, n, n_idx, rep, B, rows, hp,
                                 mcmc_seed, seed, y_sd, n_for_examples) {
  metrics <- list()
  examples <- list()
  acf_sigma <- list()
  fitted_quantile_acf_list <- list()
  fitted_quantile_joint_list <- list()

  rep_seed <- seed * 1e6 + n * 1e3 + rep
  set.seed(rep_seed)

  dgp <- if (outcome == "continuous") simulate_friedman(n, y_sd = y_sd) else simulate_friedman_binary(n)
  X <- dplyr::select(dgp$data, dplyr::starts_with("x"))
  y <- dgp$data$y
  truth <- if (outcome == "continuous") dgp$f_true else dgp$prob_true

  fitted_means <- list()
  keep_example <- (n == n_for_examples) && (rep == 1)

  for (row_index in seq_along(rows)) {
    row <- rows[[row_index]]

    # Deterministic in (outcome, n_idx, rep, row_index) - not a running
    # counter - so it's safe under out-of-order or concurrent execution
    # (parallelizing over rep). Digit budgets don't overlap for this
    # benchmark's realistic ranges (row_index <= 99, rep <= 999, n_idx <= 99),
    # well under .Machine$integer.max.
    fit_seed <- as.integer(
      mcmc_seed * 1e8 + outcome_code * 1e7 + n_idx * 1e5 + rep * 1e2 + row_index
    )

    t0 <- Sys.time()
    fit <- row$fit(X, y, hp, seed = fit_seed)
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

    # sigma is fixed at 1 under every engine's probit link for a binary
    # outcome (same restriction as the rfx benchmark) - continuous only.
    sigma_ess <- NA_real_
    if (outcome == "continuous") {
      n_draws <- dplyr::n_distinct(draws$.draw)
      sigma_draws <- extract_sigma_draws_prediction(fit, row$engine, n_draws = n_draws)
      sigma_ess <- ess_safe(sigma_draws)

      acf_tbl <- sigma_acf_prediction(fit, row$engine, n_draws = n_draws)
      if (!is.null(acf_tbl)) {
        acf_tbl$outcome <- outcome; acf_tbl$n <- n; acf_tbl$rep <- rep
        acf_tbl$engine <- row$engine; acf_tbl$variant <- row$variant
        acf_sigma[[length(acf_sigma) + 1]] <- acf_tbl
      }

      fq_acf <- fitted_quantile_acf(draws, "fit")
      if (nrow(fq_acf) > 0) {
        fq_acf$outcome <- outcome; fq_acf$n <- n; fq_acf$rep <- rep
        fq_acf$engine <- row$engine; fq_acf$variant <- row$variant
        fitted_quantile_acf_list[[length(fitted_quantile_acf_list) + 1]] <- fq_acf
      }

      # Kept for the one representative fit only (same n/rep scope as
      # `examples`), since storing every replicate's full draw-by-draw
      # values would be a lot of data for a comparison that only needs to
      # be illustrative - same principle as the rfx benchmark's own
      # fitted_quantile_joint.
      if (keep_example) {
        fq_joint <- fitted_quantile_draws(draws, "fit")
        # Only meaningful if these line up one-to-one with .draw (true
        # whenever hp$chains == 1, the only case used in this benchmark) -
        # guarded rather than assumed, since a silent length mismatch would
        # otherwise recycle nonsense into the scatter plot.
        fq_joint$sigma <- if (length(sigma_draws) == nrow(fq_joint)) sigma_draws else NA_real_
        fq_joint$outcome <- outcome; fq_joint$n <- n; fq_joint$rep <- rep
        fq_joint$engine <- row$engine; fq_joint$variant <- row$variant
        fitted_quantile_joint_list[[length(fitted_quantile_joint_list) + 1]] <- fq_joint
      }
    }

    metrics[[length(metrics) + 1]] <- dplyr::tibble(
      outcome = outcome, n = n, rep = rep,
      engine = row$engine, variant = row$variant,
      rmse = rmse(pm, truth),
      mae = mae(pm, truth),
      coverage95 = coverage(draws, "fit", truth, level = 0.95),
      crps = crps_from_draws(draws, "fit", truth),
      fit_time_sec = fit_time_sec,
      fitted_ess = fitted_value_ess(draws, "fit"),
      sigma_ess = sigma_ess
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
    examples = dplyr::bind_rows(examples),
    acf_sigma = dplyr::bind_rows(acf_sigma),
    fitted_quantile_acf = dplyr::bind_rows(fitted_quantile_acf_list),
    fitted_quantile_joint = dplyr::bind_rows(fitted_quantile_joint_list)
  )
}

# Attached/sourced by every worker before its first cell - mirrors
# benchmark-prediction.qmd's own setup chunk. See
# R/parallel-driver.R:with_worker_setup() for why this runs unconditionally
# on every call rather than being gated to run once per worker.
prediction_worker_setup <- function(benchmark_dir) {
  library(dplyr); library(tidyr); library(tidytreatment); library(tidybayes)
  library(BART); library(dbarts); library(stochtree)
  for (f in c("cache.R", "hyperparams.R", "dgp.R", "metrics.R", "metrics-prediction.R",
              "fit-engines-prediction.R", "run-benchmark-prediction.R")) {
    source(file.path(benchmark_dir, "R", f))
  }
}

# epred_draws() is called with no explicit `scale`: every engine's default
# resolves to the response scale on its own (continuous -> "linear"/no-op,
# binary -> "probability"), so no outcome-specific branching is needed here.
run_benchmark_prediction <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L, y_sd = 1,
                                      mcmc_seed = 1L, benchmark_dir = getwd()) {
  n_for_examples <- max(n_values)
  outcomes <- c("continuous", "binary")
  grid <- build_benchmark_grid(outcomes, n_values, B)

  cell_fn <- with_worker_setup(
    setup_fn = function() prediction_worker_setup(benchmark_dir),
    cell_fn = function(cell) {
      rows <- prediction_rows(cell$outcome)
      outcome_code <- match(cell$outcome, c("continuous", "binary")) - 1L
      run_prediction_cell(cell$outcome, outcome_code, cell$n, cell$n_idx, cell$rep, B, rows, hp,
                           mcmc_seed, seed, y_sd, n_for_examples)
    }
  )
  label_fn <- function(cell) paste("outcome =", cell$outcome, "n =", cell$n, "rep =", paste0(cell$rep, "/", B))

  results <- run_cells_parallel(grid, cell_fn, label_fn)

  list(
    metrics = dplyr::bind_rows(lapply(results, `[[`, "metrics")),
    agreement = dplyr::bind_rows(lapply(results, `[[`, "agreement")),
    examples = dplyr::bind_rows(lapply(results, `[[`, "examples")),
    acf_sigma = dplyr::bind_rows(lapply(results, `[[`, "acf_sigma")),
    fitted_quantile_acf = dplyr::bind_rows(lapply(results, `[[`, "fitted_quantile_acf")),
    fitted_quantile_joint = dplyr::bind_rows(lapply(results, `[[`, "fitted_quantile_joint"))
  )
}
