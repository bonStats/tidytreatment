# Part A orchestration: loops DGP (continuous/binary) x n x replication x
# engine/variant row, fitting through fit-engines-prediction.R and extracting
# through tidytreatment's own epred_draws() (not the raw model object) - this
# is deliberate: it's the tidytreatment *wrapper* being exercised/benchmarked
# here, consistent with the "side effect of checking for errors in
# implementations" goal, not just the underlying tree-fitting code.

prediction_rows <- function(outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  if (outcome == "continuous") {
    list(
      list(engine = "BART::wbart", variant = "baseline",
           fit = function(X, y, hp) fit_wbart(X, y, hp)),
      list(engine = "dbarts::bart2", variant = "baseline",
           fit = function(X, y, hp) fit_dbarts_bart(X, y, hp, outcome = "continuous")),
      list(engine = "stochtree::bart", variant = "baseline",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 0)),
      list(engine = "stochtree::bart", variant = "+gfr",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "continuous", num_gfr = 5))
    )
  } else {
    list(
      list(engine = "BART::pbart", variant = "baseline",
           fit = function(X, y, hp) fit_pbart(X, y, hp)),
      list(engine = "dbarts::bart2", variant = "baseline",
           fit = function(X, y, hp) fit_dbarts_bart(X, y, hp, outcome = "binary")),
      list(engine = "stochtree::bart", variant = "baseline",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 0)),
      list(engine = "stochtree::bart", variant = "+gfr",
           fit = function(X, y, hp) fit_stochtree_bart(X, y, hp, outcome = "binary", num_gfr = 5))
    )
  }
}

# epred_draws() is called with no explicit `scale` anywhere in this file:
# every engine's default now resolves to the response scale on its own
# (continuous -> "linear"/no-op, binary -> "probability"), so this loop
# doesn't need outcome-specific branching for it - a direct, real use of the
# package-wide default-scale behaviour, not just something checked in
# testthat.
run_benchmark_prediction <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L) {
  metrics <- list()
  agreement <- list()
  examples <- list()
  n_for_examples <- max(n_values)

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
          t0 <- Sys.time()
          fit <- row$fit(X, y, hp)
          fit_time_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

          draws <- tidybayes::epred_draws(fit, newdata = X, include_newdata = FALSE, value = "fit")
          pm <- posterior_mean_by_row(draws, "fit")
          fitted_means[[paste(row$engine, row$variant)]] <- pm

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
