# Shared, small, fast-fitting model fixtures used across multiple test files.
# Kept deliberately tiny (few trees / few posterior draws) so the whole suite
# stays fast; these are for checking *mechanics* (shapes, joins, arg
# validation), not for checking statistical performance.

# --- BART package: binary and multiclass response models ---------------

fixture_pbart <- NULL
fixture_lbart <- NULL
fixture_mbart <- NULL
fixture_mbart2 <- NULL

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

    n_multi <- 40
    x_multi <- data.frame(
      x1 = stats::rnorm(n_multi),
      x2 = stats::rnorm(n_multi)
    )
    y_multi <- sample(1:3, n_multi, replace = TRUE)

    fixture_mbart <- tryCatch(
      BART::mbart(
        x.train = x_multi, y.train = y_multi,
        ndpost = 20, nskip = 20, ntree = 10, printevery = 1000L
      ),
      error = function(e) NULL
    )

    fixture_mbart2 <- tryCatch(
      BART::mbart2(
        x.train = x_multi, y.train = y_multi,
        ndpost = 20, nskip = 20, ntree = 10, printevery = 1000L
      ),
      error = function(e) NULL
    )
  })

  fixture_bin_x <- x_bin
  fixture_bin_y <- y_bin
}

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

    fixture_stan4bart_bin <- tryCatch(
      stan4bart::stan4bart(
        zl ~ bart(x1 + x2 + x3) + (1 | subject_id),
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

    fixture_bartc <- tryCatch(
      bartCause::bartc(
        response = y, treatment = z, confounders = x1 + x2 + x3,
        parametric = (1 | subject_id),
        data = fixture_bartc_data,
        method.rsp = "bart", method.trt = "bart",
        iter = 60, chains = 2, cores = 1, n.trees = 15,
        verbose = FALSE, seed = 1
      ),
      error = function(e) NULL
    )
  })
}
