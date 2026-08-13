library(dplyr)

skip_if_not_installed("stochtree")
skip_if(is.null(fixture_bcf))
skip_if(is.null(fixture_bcf_bin))

newX <- fixture_bcf_x[1:5, ]
newZ <- fixture_bcf_z[1:5]
newPi <- fixture_bcf_pi[1:5]

# --- covariate_importance.bcfmodel ------------------------------------------

test_that("covariate_importance.bcfmodel (forest = 'treatment_effect', default) matches forests_tau split counts", {
  res <- covariate_importance(fixture_bcf, X_train = fixture_bcf_x)

  base_vars <- colnames(fixture_bcf_x)[fixture_bcf$train_set_metadata$original_var_indices]
  has_ps <- fixture_bcf$model_params$propensity_covariate %in% c("treatment_effect", "both")
  vars <- if (has_ps) c(base_vars, "propensity") else base_vars
  p <- fixture_bcf$model_params$num_treatment_covariates
  expected_counts <- fixture_bcf$forests_tau$get_aggregate_split_counts(p)

  expected <- dplyr::tibble(variable = vars, inclusion = expected_counts) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(inclusion = sum(inclusion), .groups = "drop") %>%
    dplyr::mutate(avg_inclusion = inclusion / sum(inclusion)) %>%
    dplyr::select(-inclusion) %>%
    dplyr::arrange(variable)

  expect_equal(dplyr::arrange(res, variable), expected)
})

test_that("covariate_importance.bcfmodel (forest = 'prognostic') matches forests_mu split counts", {
  res <- covariate_importance(fixture_bcf, X_train = fixture_bcf_x, forest = "prognostic")

  base_vars <- colnames(fixture_bcf_x)[fixture_bcf$train_set_metadata$original_var_indices]
  has_ps <- fixture_bcf$model_params$propensity_covariate %in% c("prognostic", "both")
  vars <- if (has_ps) c(base_vars, "propensity") else base_vars
  p <- fixture_bcf$model_params$num_prognostic_covariates
  expected_counts <- fixture_bcf$forests_mu$get_aggregate_split_counts(p)

  expected <- dplyr::tibble(variable = vars, inclusion = expected_counts) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(inclusion = sum(inclusion), .groups = "drop") %>%
    dplyr::mutate(avg_inclusion = inclusion / sum(inclusion)) %>%
    dplyr::select(-inclusion) %>%
    dplyr::arrange(variable)

  expect_equal(dplyr::arrange(res, variable), expected)
})

test_that("covariate_importance.bcfmodel requires X_train", {
  expect_error(covariate_importance(fixture_bcf))
})

# --- epred_draws.bcfmodel ----------------------------------------------------

test_that("epred_draws.bcfmodel (no newdata) matches model$y_hat_train", {
  ed <- epred_draws(fixture_bcf, value = "fitted", include_newdata = FALSE)

  n_draws <- ncol(fixture_bcf$y_hat_train)
  n_obs <- nrow(fixture_bcf$y_hat_train)
  expect_equal(nrow(ed), n_obs * n_draws)
  expect_true(all(is.na(ed$.chain)))

  check <- dplyr::as_tibble(fixture_bcf$y_hat_train, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("epred_draws.bcfmodel (with newdata) matches predict(..., terms = 'y_hat')", {
  check_matrix <- predict(fixture_bcf, X = newX, Z = newZ, propensity = newPi, terms = "y_hat", scale = "linear")

  ed <- epred_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi, value = "fitted", include_newdata = FALSE)

  check <- dplyr::as_tibble(check_matrix, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("epred_draws.bcfmodel errors informatively when newdata is supplied without treatment", {
  expect_error(
    epred_draws(fixture_bcf, newdata = newX, include_newdata = FALSE),
    "treatment"
  )
})

test_that("epred_draws.bcfmodel errors informatively when newdata is supplied without a required propensity score", {
  expect_error(
    epred_draws(fixture_bcf, newdata = newX, treatment = newZ, include_newdata = FALSE),
    "propensity"
  )
})

test_that("epred_draws.bcfmodel errors when include_newdata = TRUE and newdata is missing", {
  expect_error(
    epred_draws(fixture_bcf, include_newdata = TRUE),
    "newdata"
  )
})

test_that("epred_draws.bcfmodel scale = 'prob' matches predict(..., scale = 'probability') for a binary outcome model", {
  ed_prob <- epred_draws(fixture_bcf_bin, newdata = newX, treatment = newZ, propensity = newPi, value = "fitted", include_newdata = FALSE, scale = "prob")
  check <- predict(fixture_bcf_bin, X = newX, Z = newZ, propensity = newPi, terms = "y_hat", scale = "probability")

  check_df <- dplyr::as_tibble(check, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed_prob), check_df, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
  expect_true(all(comp$fitted >= 0 & comp$fitted <= 1))
})

# --- linpred_draws.bcfmodel --------------------------------------------------

test_that("linpred_draws.bcfmodel equals epred_draws(..., scale = 'real')", {
  ed <- epred_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi, value = "ep", include_newdata = FALSE, scale = "real")
  lp <- linpred_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi, value = "lp", include_newdata = FALSE)

  comp <- dplyr::left_join(as.data.frame(ed), as.data.frame(lp), by = c(".row", ".draw"))
  expect_equal(comp$lp, comp$ep)
})

# --- predicted_draws.bcfmodel -------------------------------------------------

test_that("predicted_draws.bcfmodel (continuous) centers on the fitted value with plausible dispersion", {
  pd <- predicted_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi, value = "pred", include_newdata = FALSE, include_fitted = TRUE, include_sigsqs = TRUE)

  expect_true(all(c("pred", ".fit", "sigsq") %in% names(pd)))
  resid <- pd$pred - pd$.fit
  expect_equal(mean(resid), 0, tolerance = 0.5)
  expect_equal(sd(resid), mean(sqrt(pd$sigsq)), tolerance = 1)
})

test_that("predicted_draws.bcfmodel (binary) draws 0/1 outcomes consistent with the fitted probability", {
  pd <- predicted_draws(fixture_bcf_bin, newdata = newX, treatment = newZ, propensity = newPi, value = "pred", include_newdata = FALSE, include_fitted = TRUE)

  expect_true(all(pd$pred %in% c(0L, 1L)))
  row_summary <- pd %>%
    dplyr::group_by(.data$.row) %>%
    dplyr::summarise(emp_mean = mean(.data$pred), fitted_prob = mean(.data$.fitted), .groups = "drop")
  expect_true(all(abs(row_summary$emp_mean - row_summary$fitted_prob) < 0.5))
})

# --- residual_draws.bcfmodel --------------------------------------------------

test_that("residual_draws.bcfmodel = response - fitted", {
  rd <- residual_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi, response = fixture_bcf_y[1:5], value = "resid", include_newdata = FALSE)
  ed <- epred_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi, value = "fitted", include_newdata = FALSE)

  comp <- dplyr::left_join(as.data.frame(rd), as.data.frame(ed), by = c(".row", ".draw"))
  expect_equal(comp$resid, fixture_bcf_y[1:5][comp$.row] - comp$fitted)
})

test_that("residual_draws.bcfmodel requires a response argument", {
  expect_error(
    residual_draws(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi),
    "response"
  )
})

# --- tidy_draws.bcfmodel ------------------------------------------------------

test_that("tidy_draws.bcfmodel returns per-draw global parameters actually sampled by the model", {
  td <- tidy_draws(fixture_bcf)

  expect_true(all(c(".chain", ".iteration", ".draw", "sigma2_global", "sigma2_leaf_mu") %in% names(td)))
  expect_equal(nrow(td), length(fixture_bcf$sigma2_global_samples))
  expect_equal(td$sigma2_global, fixture_bcf$sigma2_global_samples)
  expect_equal(td$sigma2_leaf_mu, fixture_bcf$sigma2_leaf_mu_samples)
  # this fixture doesn't sample a treatment-effect leaf scale or adaptive coding
  expect_false("sigma2_leaf_tau" %in% names(td))
})

test_that("tidy_draws.bcfmodel includes tau_0 when the intercept is sampled", {
  td <- tidy_draws(fixture_bcf)
  if (!is.null(fixture_bcf$tau_0_samples)) {
    expect_equal(td$tau_0, as.vector(fixture_bcf$tau_0_samples))
  }
})

# --- treatment_effects.bcfmodel ------------------------------------------------

test_that("treatment_effects.bcfmodel (no newdata) uses tau_hat_train directly, no subtraction", {
  te <- treatment_effects(fixture_bcf)

  expect_true("cte" %in% names(te))
  n_draws <- ncol(fixture_bcf$tau_hat_train)
  n_obs <- nrow(fixture_bcf$tau_hat_train)
  expect_equal(nrow(te), n_obs * n_draws)

  check <- dplyr::as_tibble(fixture_bcf$tau_hat_train, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "cte_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(te), check, by = c(".row", ".draw"))
  expect_equal(comp$cte, comp$cte_check)
})

test_that("treatment_effects.bcfmodel (with newdata) matches predict(..., terms = 'tau') directly", {
  te <- treatment_effects(fixture_bcf, newdata = newX, treatment = newZ, propensity = newPi)
  check_matrix <- predict(fixture_bcf, X = newX, Z = newZ, propensity = newPi, terms = "tau", scale = "linear")

  check <- dplyr::as_tibble(check_matrix, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "cte_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(te), check, by = c(".row", ".draw"))
  expect_equal(comp$cte, comp$cte_check)
})

test_that("treatment_effects.bcfmodel subset filtering requires and uses the treatment vector", {
  expect_error(
    treatment_effects(fixture_bcf, subset = "treated"),
    "treatment"
  )

  te_t <- treatment_effects(fixture_bcf, treatment = fixture_bcf_z, subset = "treated")
  te_nt <- treatment_effects(fixture_bcf, treatment = fixture_bcf_z, subset = "nontreated")
  te_all <- treatment_effects(fixture_bcf, treatment = fixture_bcf_z, subset = "all")

  expect_equal(nrow(te_t) + nrow(te_nt), nrow(te_all))
  expect_true(all(te_t$.row %in% which(fixture_bcf_z == 1)))
  expect_true(all(te_nt$.row %in% which(fixture_bcf_z == 0)))
})

test_that("treatment_effects.bcfmodel warns and ignores common_support_method (not currently supported)", {
  expect_warning(
    treatment_effects(fixture_bcf, common_support_method = "sd"),
    "not currently supported"
  )
})

test_that("avg_treatment_effects/tidy_ate/tidy_att work end-to-end with a bcfmodel via the cte convention", {
  ate <- avg_treatment_effects(fixture_bcf, treatment = newZ, newdata = newX, propensity = newPi)
  expect_true("ate" %in% names(ate))

  check_matrix <- predict(fixture_bcf, X = newX, Z = newZ, propensity = newPi, terms = "tau", scale = "linear")
  expect_equal(sort(ate$ate), sort(colMeans(check_matrix)))
})
