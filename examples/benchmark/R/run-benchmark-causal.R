# Part B row registry + extraction. Each row's fit() returns
# list(kind = "twostep"|"native", fit = <model>, newdata=, treatment_col=) -
# extract_cte_draws() below dispatches on `kind` so the rest of the pipeline
# (metrics) never needs to know which engine produced a given row.
#
# stan4bart does not appear here - see fit-engines-causal.R's header comment.

causal_row_specs <- function(outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  bart_twostep_fit <- if (outcome == "continuous") fit_bart_twostep else fit_pbart_twostep

  twostep <- function(fit_fn, ...) {
    dots <- list(...)
    function(X, y, z, hp) {
      r <- do.call(fit_fn, c(list(X = X, y = y, z = z, hp = hp), dots))
      list(kind = "twostep", fit = r$fit, newdata = r$newdata, treatment_col = r$treatment_col)
    }
  }
  native <- function(fit_fn, ...) {
    dots <- list(...)
    function(X, y, z, hp) list(kind = "native", fit = do.call(fit_fn, c(list(X = X, y = y, z = z, hp = hp), dots)))
  }

  # BART::wbart's own defaults already equal baseline (see hyperparams.R) -
  # no default row for the continuous outcome, matching benchmark-prediction.qmd.
  # BART::pbart's ntree default (50 vs 200) differs, so binary gets one.
  bart_default_rows <- if (outcome == "binary") {
    list(list(engine = "BART two-step", variant = "default", propensity_recipe = "two_stage",
         fit = twostep(fit_pbart_twostep, propensity_recipe = "two_stage", variant = "default")))
  } else {
    list()
  }

  c(list(
    list(engine = "BART two-step", variant = "baseline", propensity_recipe = "two_stage",
         fit = twostep(bart_twostep_fit, propensity_recipe = "two_stage")),
    list(engine = "BART two-step", variant = "baseline", propensity_recipe = "ps_all",
         fit = twostep(bart_twostep_fit, propensity_recipe = "ps_all"))
  ), bart_default_rows, list(

    list(engine = "stochtree::bart two-step", variant = "baseline", propensity_recipe = "two_stage",
         fit = twostep(fit_stochtree_bart_twostep, propensity_recipe = "two_stage", outcome = outcome, num_gfr = 0)),
    list(engine = "stochtree::bart two-step", variant = "baseline", propensity_recipe = "ps_all",
         fit = twostep(fit_stochtree_bart_twostep, propensity_recipe = "ps_all", outcome = outcome, num_gfr = 0)),
    list(engine = "stochtree::bart two-step", variant = "baseline+gfr", propensity_recipe = "two_stage",
         fit = twostep(fit_stochtree_bart_twostep, propensity_recipe = "two_stage", outcome = outcome, num_gfr = 5)),
    list(engine = "stochtree::bart two-step", variant = "default", propensity_recipe = "two_stage",
         fit = twostep(fit_stochtree_bart_twostep, propensity_recipe = "two_stage", outcome = outcome, variant = "default")),

    list(engine = "bartc", variant = "baseline", propensity_recipe = "two_stage",
         fit = native(fit_bartc, propensity_mode = "diy_two_stage")),
    list(engine = "bartc", variant = "baseline", propensity_recipe = "ps_all",
         fit = native(fit_bartc, propensity_mode = "diy_ps_all")),
    list(engine = "bartc", variant = "builtin_propensity", propensity_recipe = NA,
         fit = native(fit_bartc, propensity_mode = "builtin")),
    list(engine = "bartc", variant = "+auto_k", propensity_recipe = "two_stage",
         fit = native(fit_bartc, propensity_mode = "diy_two_stage", auto_k = TRUE)),
    list(engine = "bartc", variant = "default", propensity_recipe = "two_stage",
         fit = native(fit_bartc, propensity_mode = "diy_two_stage", variant = "default")),

    list(engine = "stochtree::bcf", variant = "baseline", propensity_recipe = "two_stage",
         fit = native(fit_bcf, propensity_mode = "diy_two_stage", outcome = outcome)),
    list(engine = "stochtree::bcf", variant = "baseline", propensity_recipe = "ps_all",
         fit = native(fit_bcf, propensity_mode = "diy_ps_all", outcome = outcome)),
    list(engine = "stochtree::bcf", variant = "builtin_propensity", propensity_recipe = NA,
         fit = native(fit_bcf, propensity_mode = "builtin", outcome = outcome)),
    list(engine = "stochtree::bcf", variant = "+gfr_adaptive_coding", propensity_recipe = "two_stage",
         fit = native(fit_bcf, propensity_mode = "diy_two_stage", outcome = outcome, num_gfr = 5, adaptive_coding = TRUE)),
    list(engine = "stochtree::bcf", variant = "default", propensity_recipe = "two_stage",
         fit = native(fit_bcf, propensity_mode = "diy_two_stage", outcome = outcome, variant = "default"))
  ))
}

extract_cte_draws <- function(row_result) {
  if (row_result$kind == "twostep") {
    tidytreatment::treatment_effects(row_result$fit, treatment = row_result$treatment_col, newdata = row_result$newdata)
  } else {
    tidytreatment::treatment_effects(row_result$fit)
  }
}

# One replication's worth of work: simulate one dataset, fit every row
# (engine/variant/propensity_recipe) against it, and return this cell's
# contribution to each accumulator as a named list - the unit of parallelism
# (see R/parallel-driver.R). `agreement` is NULL when fewer than two
# baseline rows produced a cte estimate (mirrors the original's conditional
# append) - dplyr::bind_rows() over the collected results silently drops
# NULL entries, so no special-casing is needed at the bind step.
run_causal_cell <- function(outcome, response_parallel, n, rep, B, rows, hp, seed, tau, y_sd, n_for_examples) {
  metrics <- list()
  common_support <- list()
  examples <- list()

  rep_seed <- seed * 1e6 + n * 1e3 + rep * 10 + response_parallel
  set.seed(rep_seed)

  sim <- simulate_su_hill_data(n = n, tau = tau, response_parallel = response_parallel, y_sd = y_sd)
  X <- dplyr::select(sim$data, dplyr::starts_with("x"))
  z <- as.integer(sim$data$z)
  y_cont <- sim$data$y

  if (outcome == "continuous") {
    y <- y_cont
    true_ite <- su_hill_true_effects(sim)$ite
    true_ate <- mean(true_ite)
  } else {
    threshold <- stats::median(y_cont)
    y <- as.integer(y_cont > threshold)
    truth_bin <- su_hill_truth_binary(sim, y_sd = y_sd, threshold = threshold)
    true_ite <- truth_bin$ite
    true_ate <- truth_bin$ate
  }
  heterogeneous <- stats::sd(true_ite) > 0
  keep_example <- heterogeneous && (n == n_for_examples) && (rep == 1)

  cte_by_row <- list()

  for (row in rows) {
    t0 <- Sys.time()
    fitted <- row$fit(X, y, z, hp)
    fit_time_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    te_draws <- extract_cte_draws(fitted)
    pm <- posterior_mean_by_row(te_draws, "cte")

    if (identical(row$variant, "baseline")) {
      key <- paste(row$engine, row$propensity_recipe)
      cte_by_row[[key]] <- pm
    }

    if (keep_example) {
      examples[[length(examples) + 1]] <- dplyr::tibble(
        outcome = outcome, engine = row$engine, variant = row$variant,
        propensity_recipe = row$propensity_recipe %||% NA_character_,
        .row = seq_along(pm), true_ite = true_ite, cte_hat = pm
      )
    }

    ate <- ate_summary(te_draws, true_ate)
    att <- att_summary(te_draws, z, true_ite)

    metrics[[length(metrics) + 1]] <- dplyr::bind_cols(
      dplyr::tibble(
        outcome = outcome, response_parallel = response_parallel, n = n, rep = rep,
        engine = row$engine, variant = row$variant, propensity_recipe = row$propensity_recipe %||% NA_character_,
        pehe = if (heterogeneous) pehe(te_draws, true_ite) else NA_real_,
        fit_time_sec = fit_time_sec
      ),
      ate, att
    )

    if (identical(row$engine, "bartc")) {
      cs_rate <- bartc_common_support_agreement(fitted$fit)
      common_support[[length(common_support) + 1]] <- dplyr::tibble(
        outcome = outcome, response_parallel = response_parallel, n = n, rep = rep,
        variant = row$variant, propensity_recipe = row$propensity_recipe %||% NA_character_,
        agreement_rate = cs_rate
      )
    }
  }

  agr <- NULL
  if (length(cte_by_row) >= 2) {
    agr <- cross_engine_agreement(cte_by_row)
    agr$outcome <- outcome
    agr$response_parallel <- response_parallel
    agr$n <- n
    agr$rep <- rep
  }

  list(
    metrics = dplyr::bind_rows(metrics),
    agreement = agr,
    common_support = dplyr::bind_rows(common_support),
    examples = dplyr::bind_rows(examples)
  )
}

# Attached/sourced by every worker before its first cell - mirrors
# benchmark-causal.qmd's own setup chunk. See
# R/parallel-driver.R:with_worker_setup() for why this runs unconditionally
# on every call rather than being gated to run once per worker.
causal_worker_setup <- function(benchmark_dir) {
  library(dplyr); library(tidyr); library(tidytreatment); library(tidybayes)
  library(bartCause); library(dbarts); library(stochtree); library(BART)
  for (f in c("cache.R", "hyperparams.R", "dgp.R", "metrics.R", "metrics-causal.R",
              "propensity.R", "fit-engines-causal.R", "run-benchmark-causal.R")) {
    source(file.path(benchmark_dir, "R", f))
  }
}

# Orchestration: DGP settings (outcome x response_parallel) x n x replication
# x the rows from causal_row_specs() (see that function for the current
# count - it now differs slightly by outcome). response_parallel = TRUE
# gives a homogeneous truth (clean ATE recovery check), FALSE gives
# heterogeneous truth (PEHE becomes meaningful). The binary-outcome variant
# thresholds y at its own median and uses su_hill_truth_binary() for
# probability-scale truth.
run_benchmark_causal <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L, tau = 4, y_sd = 1,
                                  benchmark_dir = getwd()) {
  n_for_examples <- max(n_values)
  outcomes <- c("continuous", "binary")
  grid <- build_benchmark_grid(outcomes, n_values, B, response_parallel_values = c(TRUE, FALSE))

  cell_fn <- with_worker_setup(
    setup_fn = function() causal_worker_setup(benchmark_dir),
    cell_fn = function(cell) {
      rows <- causal_row_specs(cell$outcome)
      run_causal_cell(cell$outcome, cell$response_parallel, cell$n, cell$rep, B, rows, hp, seed, tau, y_sd, n_for_examples)
    }
  )
  label_fn <- function(cell) {
    paste("outcome =", cell$outcome, "response_parallel =", cell$response_parallel,
          "n =", cell$n, "rep =", paste0(cell$rep, "/", B))
  }

  results <- run_cells_parallel(grid, cell_fn, label_fn)

  list(
    metrics = dplyr::bind_rows(lapply(results, `[[`, "metrics")),
    agreement = dplyr::bind_rows(lapply(results, `[[`, "agreement")),
    common_support = dplyr::bind_rows(lapply(results, `[[`, "common_support")),
    examples = dplyr::bind_rows(lapply(results, `[[`, "examples"))
  )
}

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
