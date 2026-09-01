# Random-effects prediction row registry + orchestration.

rfx_rows <- function(outcome = c("continuous", "binary"), stochtree_thin = 10, stochtree_burnin_multiplier = 5) {
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
  # See fit_stochtree_bart_rfx()'s own comment on thin/burnin_multiplier -
  # tests whether stochtree's lower MCMC effective sample size (see the ESS
  # section of Results) explains its prediction-RMSE gap vs stan4bart.
  # Substantially slower than the other stochtree rows (roughly
  # stochtree_thin times the sampling time) since it runs that many times
  # the raw sweeps per retained draw. stochtree_thin/stochtree_burnin_multiplier
  # are document parameters (see the YAML header) rather than fixed here.
  # Continuous only: sigma^2 (what this row investigates) is fixed at 1 for
  # binary under both engines' probit link, so there's nothing for the extra
  # sweeps to test there - just 10x the compute for no diagnostic value.
  if (outcome == "continuous") {
    rows <- c(rows, list(
      list(engine = "stochtree::bart", variant = "baseline+thin",
           fit = function(X, y, group, hp, seed) fit_stochtree_bart_rfx(X, y, group, hp, outcome = outcome, num_gfr = 0, thin = stochtree_thin, burnin_multiplier = stochtree_burnin_multiplier, seed = seed))
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

# sd_group defaults differ between outcome types because they live on
# different scales (raw y vs. the probit latent, sd 1 by construction) -
# see dgp.R.
run_benchmark_rfx <- function(n_values, B, hp = baseline_hyperparams(), seed = 1L,
                               n_groups = 20, sd_group_continuous = 3, sd_group_binary = 0.5, y_sd = 1,
                               stochtree_thin = 10, stochtree_burnin_multiplier = 5, mcmc_seed = 1L) {
  metrics <- list()
  group_metrics <- list()
  examples <- list()
  group_examples <- list()
  agreement <- list()
  acf_global <- list()
  # Named distinctly from the fitted_quantile_acf()/fitted_quantile_draws()
  # helper functions in metrics-rfx.R - a same-named local list would shadow
  # them within this function's scope.
  fitted_quantile_acf_list <- list()
  fitted_quantile_joint_list <- list()
  n_for_examples <- max(n_values)

  # continuous and binary no longer have the same row count ("baseline+thin"
  # is continuous-only), so total_fits sums each outcome's own row count
  # rather than assuming symmetry.
  n_rows_continuous <- length(rfx_rows("continuous", stochtree_thin = stochtree_thin, stochtree_burnin_multiplier = stochtree_burnin_multiplier))
  n_rows_binary <- length(rfx_rows("binary", stochtree_thin = stochtree_thin, stochtree_burnin_multiplier = stochtree_burnin_multiplier))
  total_fits <- length(n_values) * B * (n_rows_continuous + n_rows_binary)
  fit_i <- 0

  for (outcome in c("continuous", "binary")) {
    rows <- rfx_rows(outcome, stochtree_thin = stochtree_thin, stochtree_burnin_multiplier = stochtree_burnin_multiplier)

    for (n in n_values) {
      for (rep in seq_len(B)) {
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

        for (row in rows) {
          fit_i <- fit_i + 1
          progress_note(fit_i, total_fits, "outcome =", outcome, "n =", n, "rep =", paste0(rep, "/", B), "engine =", row$engine, row$variant)

          # Tied to the mcmc_seed document parameter (distinct from `seed`,
          # which governs the DGP) rather than left to each package's own
          # default - stochtree's own default (general_params$random_seed =
          # -1) seeds its internal C++ RNG from hardware entropy and ignores
          # R's set.seed() entirely, so fits were not reproducible run-to-run
          # without this (confirmed empirically). fit_i already uniquely
          # identifies this exact fit within the run.
          fit_seed <- as.integer(mcmc_seed * 1e6 + fit_i)

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
        agreement[[length(agreement) + 1]] <- agr
      }
    }
  }

  list(
    metrics = dplyr::bind_rows(metrics),
    agreement = dplyr::bind_rows(agreement),
    group_metrics = dplyr::bind_rows(group_metrics),
    examples = dplyr::bind_rows(examples),
    group_examples = dplyr::bind_rows(group_examples),
    acf_global = dplyr::bind_rows(acf_global),
    fitted_quantile_acf = dplyr::bind_rows(fitted_quantile_acf_list),
    fitted_quantile_joint = dplyr::bind_rows(fitted_quantile_joint_list)
  )
}
