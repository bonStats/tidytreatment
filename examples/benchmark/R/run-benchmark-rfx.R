# Random-effects prediction row registry + orchestration.

rfx_rows <- function(outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  rows <- list(
    list(engine = "stan4bart", variant = "baseline",
         fit = function(X, y, group, hp, seed) fit_stan4bart_rfx(X, y, group, hp, seed = seed)),
    list(engine = "stan4bart", variant = "default",
         fit = function(X, y, group, hp, seed) fit_stan4bart_rfx(X, y, group, hp, variant = "default", seed = seed)),
    list(engine = "stochtree::bart", variant = "baseline",
         fit = function(X, y, group, hp, seed) fit_stochtree_bart_rfx(X, y, group, hp, outcome = outcome, num_gfr = 0, seed = seed)),
    list(engine = "stochtree::bart", variant = "baseline+gfr",
         fit = function(X, y, group, hp, seed) fit_stochtree_bart_rfx(X, y, group, hp, outcome = outcome, num_gfr = 5, seed = seed))
  )
  # "default+sigest" is continuous-only: prior_aux (and hence any difference
  # from "default") is a complete no-op under stan4bart's binary/probit link,
  # which has no continuous residual to place a variance prior on.
  if (outcome == "continuous") {
    rows <- c(rows, list(
      list(engine = "stan4bart", variant = "default+sigest",
           fit = function(X, y, group, hp, seed) fit_stan4bart_rfx(X, y, group, hp, variant = "default+sigest", seed = seed))
    ))
  }
  c(rows, list(
    list(engine = "stochtree::bart", variant = "default",
         fit = function(X, y, group, hp, seed) fit_stochtree_bart_rfx(X, y, group, hp, outcome = outcome, variant = "default", seed = seed))
  ))
}

extract_rfx_prediction_draws <- function(fit, engine, X, group) {
  if (engine == "stan4bart") {
    tidybayes::epred_draws(fit, value = "fit")
  } else {
    tidybayes::epred_draws(fit, newdata = X, rfx_group_ids = as.integer(group), include_newdata = FALSE, value = "fit")
  }
}

extract_rfx_group_effects <- function(fit, engine, group_ids_unique) {
  if (engine == "stan4bart") {
    extract_group_effects_stan4bart(fit)
  } else {
    extract_group_effects_stochtree(fit, group_ids_unique)
  }
}

# One replication's worth of work: simulate one dataset, fit every row
# (engine/variant) against it, and return this cell's contribution to each
# of the 8 accumulators as a single named list of tibbles - the unit of
# parallelism (see R/parallel-driver.R). `outcome_code`/`n_idx` are needed
# for the deterministic fit_seed formula (see its own comment below);
# `n_for_examples`/`B` are needed for the keep_example/progress-label logic
# that used to read closure variables from the enclosing loop directly.
run_rfx_cell <- function(outcome, outcome_code, n, n_idx, rep, B, rows, hp,
                          mcmc_seed, seed, n_groups, sd_group_continuous,
                          sd_group_binary, y_sd, n_for_examples) {
  metrics <- list()
  group_metrics <- list()
  examples <- list()
  group_examples <- list()
  acf_global <- list()
  fitted_quantile_acf_list <- list()
  fitted_quantile_joint_list <- list()

  rep_seed <- seed * 1e6 + n * 1e3 + rep
  set.seed(rep_seed)

  if (outcome == "continuous") {
    sim <- simulate_friedman_rfx(n = n, n_groups = n_groups, sd_group = sd_group_continuous, y_sd = y_sd)
    truth <- sim$mu_true
  } else {
    sim <- simulate_friedman_rfx_binary(n = n, n_groups = n_groups, sd_group = sd_group_binary)
    truth <- sim$prob_true
  }
  X <- dplyr::select(sim$data, dplyr::starts_with("x"))
  y <- sim$data$y
  group <- sim$data$group
  group_ids_unique <- sort(unique(as.integer(group)))
  keep_example <- (n == n_for_examples) && (rep == 1)
  fitted_means <- list()

  for (row_index in seq_along(rows)) {
    row <- rows[[row_index]]

    # Tied to the mcmc_seed document parameter (distinct from `seed`, which
    # governs the DGP) rather than left to each package's own default -
    # stochtree's own default (general_params$random_seed = -1) seeds its
    # internal C++ RNG from hardware entropy and ignores R's set.seed()
    # entirely, so fits were not reproducible run-to-run without this
    # (confirmed empirically).
    #
    # Deterministic in (outcome, n_idx, rep, row_index) - not a running
    # counter like the old fit_i-based scheme - so it's safe under
    # out-of-order or concurrent execution (parallelizing over rep). Digit
    # budgets don't overlap for this benchmark's realistic ranges
    # (row_index <= 99, rep <= 999, n_idx <= 99), well under
    # .Machine$integer.max.
    fit_seed <- as.integer(
      mcmc_seed * 1e8 + outcome_code * 1e7 + n_idx * 1e5 + rep * 1e2 + row_index
    )

    t0 <- Sys.time()
    fit <- row$fit(X, y, group, hp, seed = fit_seed)
    fit_time_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    draws <- extract_rfx_prediction_draws(fit, row$engine, X, group)
    pm <- posterior_mean_by_row(draws, "fit")

    # Cross-engine agreement is a comparison of matched-prior fits only -
    # "default" and "baseline+gfr" rows use different priors/warm-start
    # settings and would muddy the comparison, so only "baseline" rows
    # are collected here (same principle as run_benchmark_prediction()).
    if (identical(row$variant, "baseline")) {
      fitted_means[[row$engine]] <- pm
    }

    if (keep_example) {
      examples[[length(examples) + 1]] <- dplyr::tibble(
        outcome = outcome, engine = row$engine, variant = row$variant,
        .row = seq_along(pm), truth = truth, fitted = pm
      )
    }

    # MCMC mixing diagnostics - see metrics-rfx.R's header comment for
    # why these two specific parameters and why this was worth adding.
    sigma_global_ess <- ess_safe(extract_sigma_global_draws(fit, row$engine, outcome))
    sigma_group_ess <- ess_safe(extract_sigma_group_draws(fit, row$engine))

    acf_tbl <- sigma_global_acf(fit, row$engine, outcome)
    if (!is.null(acf_tbl)) {
      acf_tbl$outcome <- outcome
      acf_tbl$n <- n
      acf_tbl$rep <- rep
      acf_tbl$engine <- row$engine
      acf_tbl$variant <- row$variant
      acf_global[[length(acf_global) + 1]] <- acf_tbl
    }

    # Follow-up check: sigma^2 and the trees both update conditional on
    # each other every sweep, so a shared bottleneck candidate is how
    # quickly the tree ensemble's own fit moves - not observable from
    # sigma^2 alone. Tracked via per-draw quantiles of the fitted
    # values (continuous only, same rationale as sigma_global_acf()).
    if (outcome == "continuous") {
      fq_acf <- fitted_quantile_acf(draws, "fit")
      if (nrow(fq_acf) > 0) {
        fq_acf$outcome <- outcome
        fq_acf$n <- n
        fq_acf$rep <- rep
        fq_acf$engine <- row$engine
        fq_acf$variant <- row$variant
        fitted_quantile_acf_list[[length(fitted_quantile_acf_list) + 1]] <- fq_acf
      }

      # Joint per-draw values (quantiles alongside sigma/sigma_G, both
      # on the SD scale - see extract_sigma_global_draws()'s header
      # comment) for the pairwise-correlation check - kept for the one
      # representative fit only (same n/rep scope as
      # examples/group_examples), since storing every replicate's full
      # draw-by-draw values would be a lot of data for a comparison
      # that only needs to be illustrative.
      if (keep_example) {
        fq_joint <- fitted_quantile_draws(draws, "fit")
        sigma_draws <- extract_sigma_global_draws(fit, row$engine, outcome)
        sigma_g_draws <- extract_sigma_group_draws(fit, row$engine)
        # Only meaningful if these line up one-to-one with .draw (true
        # whenever hp$chains == 1, the only case used in this
        # benchmark) - guarded rather than assumed, since a silent
        # length mismatch would otherwise recycle nonsense into the
        # scatter plot.
        fq_joint$sigma <- if (length(sigma_draws) == nrow(fq_joint)) sigma_draws else NA_real_
        fq_joint$sigma_G <- if (length(sigma_g_draws) == nrow(fq_joint)) sigma_g_draws else NA_real_
        fq_joint$outcome <- outcome
        fq_joint$n <- n
        fq_joint$rep <- rep
        fq_joint$engine <- row$engine
        fq_joint$variant <- row$variant
        fitted_quantile_joint_list[[length(fitted_quantile_joint_list) + 1]] <- fq_joint
      }
    }

    metrics[[length(metrics) + 1]] <- dplyr::tibble(
      outcome = outcome, n = n, rep = rep, engine = row$engine, variant = row$variant,
      rmse = rmse(pm, truth),
      mae = mae(pm, truth),
      coverage95 = coverage(draws, "fit", truth),
      crps = crps_from_draws(draws, "fit", truth),
      fit_time_sec = fit_time_sec,
      sigma_global_ess = sigma_global_ess,
      sigma_group_ess = sigma_group_ess
    )

    # Not every group is guaranteed to appear in a given sample, so ge
    # only has one estimate per *observed* group - restrict true_ge to match.
    ge <- extract_rfx_group_effects(fit, row$engine, group_ids_unique)
    true_ge <- sim$group_effect[group_ids_unique]
    gr <- group_effect_recovery(ge, true_ge)
    gr$outcome <- outcome
    gr$n <- n
    gr$rep <- rep
    gr$engine <- row$engine
    gr$variant <- row$variant
    group_metrics[[length(group_metrics) + 1]] <- gr

    if (keep_example) {
      group_examples[[length(group_examples) + 1]] <- dplyr::tibble(
        outcome = outcome, engine = row$engine, variant = row$variant,
        group_id = group_ids_unique, true_group_effect = true_ge, group_effect_hat = ge
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
    group_metrics = dplyr::bind_rows(group_metrics),
    examples = dplyr::bind_rows(examples),
    group_examples = dplyr::bind_rows(group_examples),
    acf_global = dplyr::bind_rows(acf_global),
    fitted_quantile_acf = dplyr::bind_rows(fitted_quantile_acf_list),
    fitted_quantile_joint = dplyr::bind_rows(fitted_quantile_joint_list)
  )
}

# Attached/sourced by every worker before its first cell - mirrors
# benchmark-prediction-rfx.qmd's own setup chunk. See
# R/parallel-driver.R:with_worker_setup() for why this runs unconditionally
# on every call rather than being gated to run once per worker.
rfx_worker_setup <- function(benchmark_dir) {
  library(dplyr); library(tidyr); library(tidytreatment); library(tidybayes)
  library(stan4bart); library(stochtree); library(dbarts); library(coda)
  for (f in c("cache.R", "hyperparams.R", "dgp.R", "metrics.R",
              "fit-engines-rfx.R", "metrics-rfx.R", "run-benchmark-rfx.R")) {
    source(file.path(benchmark_dir, "R", f))
  }
}

# sd_group defaults differ between outcome types because they live on
# different scales (raw y vs. the probit latent, sd 1 by construction) -
# see dgp.R.
run_benchmark_rfx <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L,
                               n_groups = 20, sd_group_continuous = 3, sd_group_binary = 0.5, y_sd = 1,
                               mcmc_seed = 1L, benchmark_dir = getwd()) {
  n_for_examples <- max(n_values)
  outcomes <- c("continuous", "binary")
  grid <- build_benchmark_grid(outcomes, n_values, B)

  cell_fn <- with_worker_setup(
    setup_fn = function() rfx_worker_setup(benchmark_dir),
    cell_fn = function(cell) {
      outcome <- cell$outcome
      rows <- rfx_rows(outcome)
      outcome_code <- match(outcome, c("continuous", "binary")) - 1L
      run_rfx_cell(outcome, outcome_code, cell$n, cell$n_idx, cell$rep, B, rows, hp,
                   mcmc_seed, seed, n_groups, sd_group_continuous, sd_group_binary, y_sd,
                   n_for_examples)
    }
  )
  label_fn <- function(cell) paste("outcome =", cell$outcome, "n =", cell$n, "rep =", paste0(cell$rep, "/", B))

  results <- run_cells_parallel(grid, cell_fn, label_fn)

  list(
    metrics = dplyr::bind_rows(lapply(results, `[[`, "metrics")),
    agreement = dplyr::bind_rows(lapply(results, `[[`, "agreement")),
    group_metrics = dplyr::bind_rows(lapply(results, `[[`, "group_metrics")),
    examples = dplyr::bind_rows(lapply(results, `[[`, "examples")),
    group_examples = dplyr::bind_rows(lapply(results, `[[`, "group_examples")),
    acf_global = dplyr::bind_rows(lapply(results, `[[`, "acf_global")),
    fitted_quantile_acf = dplyr::bind_rows(lapply(results, `[[`, "fitted_quantile_acf")),
    fitted_quantile_joint = dplyr::bind_rows(lapply(results, `[[`, "fitted_quantile_joint"))
  )
}
