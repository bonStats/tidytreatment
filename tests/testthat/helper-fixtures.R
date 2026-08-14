# Shared, small, fast-fitting model fixtures used across multiple test files.
# Kept deliberately tiny (few trees / few posterior draws) so the whole suite
# stays fast; these are for checking *mechanics* (shapes, joins, arg
# validation), not for checking statistical performance.

# --- BART package: binary and multiclass response models ---------------

fixture_pbart <- NULL
fixture_lbart <- NULL

if (requireNamespace("BART", quietly = TRUE)) {
  withr::with_seed(321, {
    n_bin <- 40
    x_bin <- data.frame(
      x1 = stats::rnorm(n_bin),
      x2 = stats::rnorm(n_bin),
      x3 = stats::rnorm(n_bin)
    )
    y_bin <- stats::rbinom(n_bin, 1, stats::plogis(x_bin$x1))

    fixture_pbart <- BART::pbart(
      x.train = x_bin, y.train = y_bin,
      ndpost = 20, nskip = 20, ntree = 10, printevery = 1000L
    )

    fixture_lbart <- BART::lbart(
      x.train = x_bin, y.train = y_bin,
      ndpost = 20, nskip = 20, ntree = 10, printevery = 1000L
    )
  })

  fixture_bin_x <- x_bin
  fixture_bin_y <- y_bin
}

# mbart/mbart2 (multinomial BART) are unsupported (see stop_mbart_unsupported()
# in R/helper.R) - no fixture model needed, a bare object with the right class
# is enough to exercise the error path.

# --- stan4bart: gaussian and bernoulli, with random intercept -----------

fixture_stan4bart <- NULL
fixture_stan4bart_bin <- NULL
fixture_stan4bart_data <- NULL

if (requireNamespace("stan4bart", quietly = TRUE)) {
  withr::with_seed(654, {
    fixture_stan4bart_data <- simulate_su_hill_data(
      n = 50, treatment_linear = FALSE, omega = 0,
      n_subjects = 5, sd_subjects = 0.3
    )$data
    fixture_stan4bart_data$zl <- as.logical(fixture_stan4bart_data$z)

    fixture_stan4bart <- tryCatch(
      stan4bart::stan4bart(
        y ~ bart(x1 + x2 + x3 + z) + (1 | subject_id),
        data = fixture_stan4bart_data,
        iter = 60, chains = 2, cores = 1, verbose = -1,
        bart_args = list(keepTrees = TRUE)
      ),
      error = function(e) NULL
    )

    # includes a fixed-effect term (x2) so that predict(..., type = "indiv.fixef")
    # is well-defined when tested with newdata; a bart()/ranef-only formula
    # has no fixed effect terms and predict() errors on that combination
    fixture_stan4bart_bin <- tryCatch(
      stan4bart::stan4bart(
        zl ~ x2 + bart(x1 + x3) + (1 | subject_id),
        data = fixture_stan4bart_data,
        iter = 60, chains = 2, cores = 1, verbose = -1,
        bart_args = list(keepTrees = TRUE)
      ),
      error = function(e) NULL
    )
  })
}

# --- bartCause: response model routed through stan4bart (via `parametric`) ---
# NOTE: bartc()'s default (no `parametric`) fits response/assignment models
# with dbarts::bart2 (class "bart"), for which no epred_draws method is
# registered here (or in tidybayes without extra deps). Supplying a
# `parametric` random-effects term routes bartc() through stan4bart instead,
# giving fit.rsp / fit.trt objects of class "stan4bartFit", which tidytreatment
# already supports.

fixture_bartc <- NULL
fixture_bartc_data <- NULL

if (requireNamespace("bartCause", quietly = TRUE) && requireNamespace("lme4", quietly = TRUE)) {
  withr::with_seed(987, {
    fixture_bartc_data <- simulate_su_hill_data(
      n = 40, treatment_linear = FALSE, omega = 0,
      n_subjects = 5, sd_subjects = 0.3
    )$data

    # args.rsp/args.trt$bart_args$keepTrees = TRUE is needed so the underlying
    # stan4bart sub-models can predict on newdata (see fixture_stan4bart above)
    fixture_bartc <- tryCatch(
      bartCause::bartc(
        response = y, treatment = z, confounders = x1 + x2 + x3,
        parametric = (1 | subject_id),
        data = fixture_bartc_data,
        method.rsp = "bart", method.trt = "bart",
        args.rsp = list(bart_args = list(keepTrees = TRUE)),
        args.trt = list(bart_args = list(keepTrees = TRUE)),
        iter = 60, chains = 2, cores = 1, n.trees = 15,
        verbose = FALSE, seed = 1
      ),
      error = function(e) NULL
    )
  })
}

# --- stochtree: continuous and binary (probit) outcome models -----------

fixture_stochtree <- NULL
fixture_stochtree_bin <- NULL
fixture_stochtree_x <- NULL
fixture_stochtree_y <- NULL
fixture_stochtree_yb <- NULL

if (requireNamespace("stochtree", quietly = TRUE)) {
  withr::with_seed(159, {
    n_st <- 60
    fixture_stochtree_x <- data.frame(
      x1 = stats::rnorm(n_st),
      x2 = stats::rnorm(n_st),
      x3 = stats::rnorm(n_st)
    )
    fixture_stochtree_y <- fixture_stochtree_x$x1 + 0.5 * fixture_stochtree_x$x2 + stats::rnorm(n_st, sd = 0.5)
    fixture_stochtree_yb <- stats::rbinom(n_st, 1, stats::plogis(fixture_stochtree_x$x1))

    fixture_stochtree <- tryCatch(
      stochtree::bart(
        X_train = fixture_stochtree_x, y_train = fixture_stochtree_y,
        num_gfr = 0, num_burnin = 10, num_mcmc = 20
      ),
      error = function(e) NULL
    )

    fixture_stochtree_bin <- tryCatch(
      stochtree::bart(
        X_train = fixture_stochtree_x, y_train = as.integer(fixture_stochtree_yb),
        general_params = list(outcome_model = stochtree::OutcomeModel(outcome = "binary")),
        num_gfr = 0, num_burnin = 10, num_mcmc = 20
      ),
      error = function(e) NULL
    )
  })
}

# --- stochtree: bcf (causal forest), continuous and binary (probit) outcome ---

fixture_bcf <- NULL
fixture_bcf_bin <- NULL
fixture_bcf_x <- NULL
fixture_bcf_z <- NULL
fixture_bcf_pi <- NULL
fixture_bcf_y <- NULL
fixture_bcf_yb <- NULL

if (requireNamespace("stochtree", quietly = TRUE)) {
  withr::with_seed(753, {
    n_bcf <- 60
    fixture_bcf_x <- data.frame(
      x1 = stats::rnorm(n_bcf),
      x2 = stats::rnorm(n_bcf),
      x3 = stats::rnorm(n_bcf)
    )
    fixture_bcf_pi <- stats::plogis(fixture_bcf_x$x1)
    fixture_bcf_z <- stats::rbinom(n_bcf, 1, fixture_bcf_pi)
    tau_x <- 1 + 0.5 * fixture_bcf_x$x2
    mu_x <- fixture_bcf_x$x1 + 0.3 * fixture_bcf_x$x3
    fixture_bcf_y <- mu_x + tau_x * fixture_bcf_z + stats::rnorm(n_bcf, sd = 0.3)
    fixture_bcf_yb <- stats::rbinom(n_bcf, 1, stats::plogis(mu_x + tau_x * fixture_bcf_z))

    fixture_bcf <- tryCatch(
      stochtree::bcf(
        X_train = fixture_bcf_x, Z_train = fixture_bcf_z, y_train = fixture_bcf_y,
        propensity_train = fixture_bcf_pi,
        num_gfr = 5, num_burnin = 5, num_mcmc = 20
      ),
      error = function(e) NULL
    )

    fixture_bcf_bin <- tryCatch(
      stochtree::bcf(
        X_train = fixture_bcf_x, Z_train = fixture_bcf_z, y_train = as.integer(fixture_bcf_yb),
        propensity_train = fixture_bcf_pi,
        general_params = list(outcome_model = stochtree::OutcomeModel(outcome = "binary")),
        num_gfr = 5, num_burnin = 5, num_mcmc = 20
      ),
      error = function(e) NULL
    )
  })
}

# --- stochtree: random-effects fixtures (bart and bcf) -----------------------
# Group effects are large relative to the forest/noise signal so decomposition
# tests can't pass by numerical accident.

fixture_stochtree_rfx <- NULL
fixture_stochtree_rfx_x <- NULL
fixture_stochtree_rfx_group <- NULL
fixture_stochtree_rfx_y <- NULL

fixture_bcf_rfx_intercept <- NULL
fixture_bcf_rfx_ipt <- NULL # intercept_plus_treatment
fixture_bcf_rfx_x <- NULL
fixture_bcf_rfx_z <- NULL
fixture_bcf_rfx_pi <- NULL
fixture_bcf_rfx_group <- NULL
fixture_bcf_rfx_y_intercept <- NULL
fixture_bcf_rfx_y_ipt <- NULL

if (requireNamespace("stochtree", quietly = TRUE)) {
  withr::with_seed(864, {
    n_rfx <- 60
    fixture_stochtree_rfx_x <- data.frame(
      x1 = stats::rnorm(n_rfx),
      x2 = stats::rnorm(n_rfx)
    )
    fixture_stochtree_rfx_group <- rep(c(1L, 2L, 3L), length.out = n_rfx)
    group_effect <- c(50, -50, 10)[fixture_stochtree_rfx_group]
    fixture_stochtree_rfx_y <- fixture_stochtree_rfx_x$x1 + group_effect + stats::rnorm(n_rfx, sd = 0.3)

    fixture_stochtree_rfx <- tryCatch(
      stochtree::bart(
        X_train = fixture_stochtree_rfx_x, y_train = fixture_stochtree_rfx_y,
        rfx_group_ids_train = fixture_stochtree_rfx_group,
        random_effects_params = list(model_spec = "intercept_only"),
        num_gfr = 5, num_burnin = 5, num_mcmc = 20
      ),
      error = function(e) NULL
    )

    n_bcf_rfx <- 80
    fixture_bcf_rfx_x <- data.frame(
      x1 = stats::rnorm(n_bcf_rfx),
      x2 = stats::rnorm(n_bcf_rfx)
    )
    fixture_bcf_rfx_pi <- rep(0.5, n_bcf_rfx)
    fixture_bcf_rfx_z <- stats::rbinom(n_bcf_rfx, 1, fixture_bcf_rfx_pi)
    fixture_bcf_rfx_group <- rep(c(1L, 2L), length.out = n_bcf_rfx)

    # intercept_only: group affects the outcome only, not the treatment effect
    intercept_effect <- c(50, -50)[fixture_bcf_rfx_group]
    tau_const <- 1.5
    fixture_bcf_rfx_y_intercept <- fixture_bcf_rfx_x$x1 + tau_const * fixture_bcf_rfx_z +
      intercept_effect + stats::rnorm(n_bcf_rfx, sd = 0.3)

    fixture_bcf_rfx_intercept <- tryCatch(
      stochtree::bcf(
        X_train = fixture_bcf_rfx_x, Z_train = fixture_bcf_rfx_z, y_train = fixture_bcf_rfx_y_intercept,
        propensity_train = fixture_bcf_rfx_pi,
        rfx_group_ids_train = fixture_bcf_rfx_group,
        random_effects_params = list(model_spec = "intercept_only"),
        num_gfr = 5, num_burnin = 5, num_mcmc = 20
      ),
      error = function(e) NULL
    )

    # intercept_plus_treatment: group also shifts the treatment effect itself
    group_tau_shift <- c(50, -50)[fixture_bcf_rfx_group]
    fixture_bcf_rfx_y_ipt <- fixture_bcf_rfx_x$x1 + (tau_const + group_tau_shift) * fixture_bcf_rfx_z +
      stats::rnorm(n_bcf_rfx, sd = 0.3)

    fixture_bcf_rfx_ipt <- tryCatch(
      stochtree::bcf(
        X_train = fixture_bcf_rfx_x, Z_train = fixture_bcf_rfx_z, y_train = fixture_bcf_rfx_y_ipt,
        propensity_train = fixture_bcf_rfx_pi,
        rfx_group_ids_train = fixture_bcf_rfx_group,
        random_effects_params = list(model_spec = "intercept_plus_treatment"),
        num_gfr = 5, num_burnin = 5, num_mcmc = 20
      ),
      error = function(e) NULL
    )
  })
}
