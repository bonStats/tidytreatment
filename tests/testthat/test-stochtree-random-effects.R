library(dplyr)

skip_if_not_installed("stochtree")

# =============================================================================
# bart() + random effects (model_spec = "intercept_only")
# =============================================================================

skip_if(is.null(fixture_stochtree_rfx))

newX_bart <- fixture_stochtree_rfx_x[1:6, ]
newGroup_bart <- fixture_stochtree_rfx_group[1:6]

test_that("bart+rfx: predict() decomposition y_hat == mean_forest + rfx, extracted directly from predict()", {
  y_hat_pred <- predict(fixture_stochtree_rfx, X = newX_bart, rfx_group_ids = newGroup_bart, terms = "y_hat")
  mean_forest_pred <- predict(fixture_stochtree_rfx, X = newX_bart, terms = "mean_forest")
  rfx_pred <- predict(fixture_stochtree_rfx, X = newX_bart, rfx_group_ids = newGroup_bart, terms = "rfx")

  expect_equal(y_hat_pred, mean_forest_pred + rfx_pred)
})

test_that("bart+rfx: in-sample y_hat_train == mean_forest(X_train) + rfx_preds_train, reconstructed from model outputs", {
  mean_forest_train <- predict(fixture_stochtree_rfx, X = fixture_stochtree_rfx_x, terms = "mean_forest")

  expect_equal(fixture_stochtree_rfx$y_hat_train, mean_forest_train + fixture_stochtree_rfx$rfx_preds_train)
})

test_that("bart+rfx: epred_draws() (no newdata) matches y_hat_train exactly", {
  ed <- epred_draws(fixture_stochtree_rfx, value = "fitted", include_newdata = FALSE)

  check <- dplyr::as_tibble(fixture_stochtree_rfx$y_hat_train, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("bart+rfx: epred_draws() (with newdata + rfx_group_ids) matches predict(..., terms = 'y_hat') directly", {
  ed <- epred_draws(fixture_stochtree_rfx, newdata = newX_bart, rfx_group_ids = newGroup_bart, value = "fitted", include_newdata = FALSE)
  check_matrix <- predict(fixture_stochtree_rfx, X = newX_bart, rfx_group_ids = newGroup_bart, terms = "y_hat")

  check <- dplyr::as_tibble(check_matrix, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("bart+rfx: epred_draws() errors informatively when newdata is supplied without rfx_group_ids", {
  expect_error(
    epred_draws(fixture_stochtree_rfx, newdata = newX_bart, include_newdata = FALSE),
    "random effects"
  )
})

test_that("bart+rfx: predicted_draws()/residual_draws() work with rfx_group_ids and stay consistent with epred_draws()", {
  pd <- predicted_draws(fixture_stochtree_rfx, newdata = newX_bart, rfx_group_ids = newGroup_bart, value = "pred", include_newdata = FALSE, include_fitted = TRUE)
  expect_true(all(c("pred", ".fit") %in% names(pd)))
  expect_equal(mean(pd$pred - pd$.fit), 0, tolerance = 1)

  rd <- residual_draws(fixture_stochtree_rfx, newdata = newX_bart, rfx_group_ids = newGroup_bart, response = fixture_stochtree_rfx_y[1:6], value = "resid", include_newdata = FALSE)
  ed <- epred_draws(fixture_stochtree_rfx, newdata = newX_bart, rfx_group_ids = newGroup_bart, value = "fitted", include_newdata = FALSE)
  comp <- dplyr::left_join(as.data.frame(rd), as.data.frame(ed), by = c(".row", ".draw"))
  expect_equal(comp$resid, fixture_stochtree_rfx_y[1:6][comp$.row] - comp$fitted)
})

# =============================================================================
# bcf() + random effects (model_spec = "intercept_only": rfx affects outcome only)
# =============================================================================

skip_if(is.null(fixture_bcf_rfx_intercept))

newX_bcf <- fixture_bcf_rfx_x[1:6, ]
newZ_bcf <- fixture_bcf_rfx_z[1:6]
newPi_bcf <- fixture_bcf_rfx_pi[1:6]
newGroup_bcf <- fixture_bcf_rfx_group[1:6]

test_that("bcf+rfx(intercept_only): predict() decomposition y_hat == mu + tau*Z + rfx, extracted directly from predict()", {
  y_hat_pred <- predict(fixture_bcf_rfx_intercept, X = newX_bcf, Z = newZ_bcf, propensity = newPi_bcf, rfx_group_ids = newGroup_bcf, terms = "y_hat")
  mu_pred <- predict(fixture_bcf_rfx_intercept, X = newX_bcf, Z = newZ_bcf, propensity = newPi_bcf, rfx_group_ids = newGroup_bcf, terms = "mu")
  tau_pred <- predict(fixture_bcf_rfx_intercept, X = newX_bcf, Z = newZ_bcf, propensity = newPi_bcf, rfx_group_ids = newGroup_bcf, terms = "tau")
  rfx_pred <- predict(fixture_bcf_rfx_intercept, X = newX_bcf, Z = newZ_bcf, propensity = newPi_bcf, rfx_group_ids = newGroup_bcf, terms = "rfx")

  expect_equal(y_hat_pred, mu_pred + tau_pred * newZ_bcf + rfx_pred)
})

test_that("bcf+rfx(intercept_only): in-sample y_hat_train == mu_hat_train + tau_hat_train*Z + rfx_preds_train, purely from stored model outputs", {
  recon <- fixture_bcf_rfx_intercept$mu_hat_train +
    fixture_bcf_rfx_intercept$tau_hat_train * fixture_bcf_rfx_z +
    fixture_bcf_rfx_intercept$rfx_preds_train

  expect_equal(fixture_bcf_rfx_intercept$y_hat_train, recon)
})

test_that("bcf+rfx(intercept_only): treatment_effects() (no newdata) uses tau_hat_train directly - unaffected by the random intercept", {
  te <- treatment_effects(fixture_bcf_rfx_intercept)

  check <- dplyr::as_tibble(fixture_bcf_rfx_intercept$tau_hat_train, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "cte_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(te), check, by = c(".row", ".draw"))
  expect_equal(comp$cte, comp$cte_check)
})

test_that("bcf+rfx(intercept_only): epred_draws() (no newdata) matches y_hat_train exactly", {
  ed <- epred_draws(fixture_bcf_rfx_intercept, value = "fitted", include_newdata = FALSE)

  check <- dplyr::as_tibble(fixture_bcf_rfx_intercept$y_hat_train, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("bcf+rfx(intercept_only): epred_draws() (with newdata) matches predict(..., terms = 'y_hat') directly", {
  ed <- epred_draws(fixture_bcf_rfx_intercept, newdata = newX_bcf, treatment = newZ_bcf, propensity = newPi_bcf, rfx_group_ids = newGroup_bcf, value = "fitted", include_newdata = FALSE)
  check_matrix <- predict(fixture_bcf_rfx_intercept, X = newX_bcf, Z = newZ_bcf, propensity = newPi_bcf, rfx_group_ids = newGroup_bcf, terms = "y_hat")

  check <- dplyr::as_tibble(check_matrix, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("bcf+rfx: epred_draws()/treatment_effects() error informatively when newdata is supplied without rfx_group_ids", {
  expect_error(
    epred_draws(fixture_bcf_rfx_intercept, newdata = newX_bcf, treatment = newZ_bcf, propensity = newPi_bcf, include_newdata = FALSE),
    "random effects"
  )
  expect_error(
    treatment_effects(fixture_bcf_rfx_intercept, newdata = newX_bcf, treatment = newZ_bcf, propensity = newPi_bcf),
    "random effects"
  )
})

# =============================================================================
# bcf() + random effects (model_spec = "intercept_plus_treatment": rfx also
# shifts the treatment effect itself - the case that needs "cate" not "tau")
# =============================================================================

skip_if(is.null(fixture_bcf_rfx_ipt))

newX_ipt <- fixture_bcf_rfx_x[1:6, ]
newZ_ipt <- fixture_bcf_rfx_z[1:6]
newPi_ipt <- fixture_bcf_rfx_pi[1:6]
newGroup_ipt <- fixture_bcf_rfx_group[1:6]

test_that("bcf+rfx(intercept_plus_treatment): predict() decomposition y_hat == mu + tau*Z + rfx still holds", {
  y_hat_pred <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "y_hat")
  mu_pred <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "mu")
  tau_pred <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "tau")
  rfx_pred <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "rfx")

  expect_equal(y_hat_pred, mu_pred + tau_pred * newZ_ipt + rfx_pred)
})

test_that("bcf+rfx(intercept_plus_treatment): 'tau' never includes the random slope on treatment, but 'cate' == tau + (slope-only rfx contribution, rescaled to the outcome scale)", {
  tau_pred <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "tau")
  cate_pred <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "cate")

  # isolate JUST the random slope-on-treatment via a custom [0, 1] rfx basis
  # (rfx_samples$predict() operates on the internal standardized-y scale, so
  # it must be rescaled by outcome_scale to match predict()'s output scale)
  slope_basis <- matrix(c(0, 1), nrow = length(newGroup_ipt), ncol = 2, byrow = TRUE)
  slope_only <- fixture_bcf_rfx_ipt$rfx_samples$predict(rfx_group_ids = newGroup_ipt, rfx_basis = slope_basis)

  expect_equal(cate_pred, tau_pred + slope_only * fixture_bcf_rfx_ipt$model_params$outcome_scale)
  # sanity: the slope contribution is not (numerically) zero, i.e. this
  # fixture actually exercises a case where cate genuinely differs from tau
  expect_true(max(abs(cate_pred - tau_pred)) > 1)
})

test_that("bcf+rfx(intercept_plus_treatment): treatment_effects() (no newdata) errors - no in-sample 'cate_hat_train' exists", {
  expect_error(
    treatment_effects(fixture_bcf_rfx_ipt),
    "intercept_plus_treatment"
  )
})

test_that("bcf+rfx(intercept_plus_treatment): treatment_effects() (with newdata) uses 'cate', not 'tau', and matches predict() directly", {
  te <- treatment_effects(fixture_bcf_rfx_ipt, newdata = newX_ipt, treatment = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt)
  check_matrix <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "cate")

  check <- dplyr::as_tibble(check_matrix, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "cte_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(te), check, by = c(".row", ".draw"))
  expect_equal(comp$cte, comp$cte_check)

  # and confirm this is NOT the same as (the wrong answer of) using "tau"
  tau_check_matrix <- predict(fixture_bcf_rfx_ipt, X = newX_ipt, Z = newZ_ipt, propensity = newPi_ipt, rfx_group_ids = newGroup_ipt, terms = "tau")
  expect_true(max(abs(check_matrix - tau_check_matrix)) > 1)
})
