# Random-effects prediction row registry + orchestration.

rfx_rows <- function(outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  list(
    list(engine = "stan4bart", variant = "baseline",
         fit = function(X, y, group, hp) fit_stan4bart_rfx(X, y, group, hp, auto_k = FALSE)),
    list(engine = "stan4bart", variant = "+auto_k",
         fit = function(X, y, group, hp) fit_stan4bart_rfx(X, y, group, hp, auto_k = TRUE)),
    list(engine = "stochtree::bart", variant = "baseline",
         fit = function(X, y, group, hp) fit_stochtree_bart_rfx(X, y, group, hp, outcome = outcome, num_gfr = 0)),
    list(engine = "stochtree::bart", variant = "baseline+gfr",
         fit = function(X, y, group, hp) fit_stochtree_bart_rfx(X, y, group, hp, outcome = outcome, num_gfr = 5))
  )
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

# sd_group defaults differ between outcome types because they live on
# different scales (raw y vs. the probit latent, sd 1 by construction) -
# see dgp.R.
run_benchmark_rfx <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L,
                               n_groups = 20, sd_group_continuous = 3, sd_group_binary = 0.5) {
  metrics <- list()
  group_metrics <- list()
  examples <- list()
  group_examples <- list()
  n_for_examples <- max(n_values)

  n_rows <- length(rfx_rows("continuous"))
  total_fits <- 2 * length(n_values) * B * n_rows
  fit_i <- 0

  for (outcome in c("continuous", "binary")) {
    rows <- rfx_rows(outcome)

    for (n in n_values) {
      for (rep in seq_len(B)) {
        rep_seed <- seed * 1e6 + n * 1e3 + rep
        set.seed(rep_seed)

        if (outcome == "continuous") {
          sim <- simulate_friedman_rfx(n = n, n_groups = n_groups, sd_group = sd_group_continuous)
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

        for (row in rows) {
          fit_i <- fit_i + 1
          progress_note(fit_i, total_fits, "outcome =", outcome, "n =", n, "rep =", paste0(rep, "/", B), "engine =", row$engine, row$variant)

          t0 <- Sys.time()
          fit <- row$fit(X, y, group, hp)
          fit_time_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

          draws <- extract_rfx_prediction_draws(fit, row$engine, X, group)
          pm <- posterior_mean_by_row(draws, "fit")

          if (keep_example) {
            examples[[length(examples) + 1]] <- dplyr::tibble(
              outcome = outcome, engine = row$engine, variant = row$variant,
              .row = seq_along(pm), truth = truth, fitted = pm
            )
          }

          metrics[[length(metrics) + 1]] <- dplyr::tibble(
            outcome = outcome, n = n, rep = rep, engine = row$engine, variant = row$variant,
            rmse = rmse(pm, truth),
            mae = mae(pm, truth),
            coverage95 = coverage(draws, "fit", truth),
            crps = crps_from_draws(draws, "fit", truth),
            fit_time_sec = fit_time_sec
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
      }
    }
  }

  list(
    metrics = dplyr::bind_rows(metrics),
    group_metrics = dplyr::bind_rows(group_metrics),
    examples = dplyr::bind_rows(examples),
    group_examples = dplyr::bind_rows(group_examples)
  )
}
