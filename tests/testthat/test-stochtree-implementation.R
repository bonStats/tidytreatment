library(dplyr)

skip_if_not_installed("stochtree")
skip_if(is.null(fixture_stochtree))
skip_if(is.null(fixture_stochtree_bin))

newX <- fixture_stochtree_x[1:5, ]

# --- epred_draws.bartmodel ------------------------------------------------

test_that("epred_draws.bartmodel (no newdata) matches model$y_hat_train", {
  ed <- epred_draws(fixture_stochtree, value = "fitted", include_newdata = FALSE)

  n_draws <- ncol(fixture_stochtree$y_hat_train)
  n_obs <- nrow(fixture_stochtree$y_hat_train)

  expect_equal(nrow(ed), n_obs * n_draws)
  expect_equal(sort(unique(ed$.row)), 1:n_obs)
  expect_equal(sort(unique(ed$.draw)), 1:n_draws)
  expect_true(all(is.na(ed$.chain)))
  expect_true(all(is.na(ed$.iteration)))

  check <- dplyr::as_tibble(fixture_stochtree$y_hat_train, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("epred_draws.bartmodel (with newdata) matches predict(..., terms = 'y_hat')", {
  check_matrix <- predict(fixture_stochtree, X = newX, terms = "y_hat", scale = "linear")

  ed <- epred_draws(fixture_stochtree, newdata = newX, value = "fitted", include_newdata = FALSE)

  check <- dplyr::as_tibble(check_matrix, .name_repair = function(nm) paste0(".d", seq_along(nm))) %>%
    dplyr::mutate(.row = dplyr::row_number()) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with(".d"), names_to = ".draw", values_to = "fitted_check") %>%
    dplyr::mutate(.draw = as.integer(gsub(".d", "", .draw)))

  comp <- dplyr::left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fitted, comp$fitted_check)
})

test_that("epred_draws.bartmodel errors when include_newdata = TRUE and newdata is missing", {
  expect_error(
    epred_draws(fixture_stochtree, include_newdata = TRUE),
    "newdata"
  )
})

test_that("epred_draws.bartmodel does not attach newdata or warn by default", {
  ed <- epred_draws(fixture_stochtree, newdata = newX, value = "fitted")
  expect_false(any(colnames(newX) %in% names(ed)))

  expect_no_warning(
    epred_draws(fixture_stochtree, newdata = newX, value = "fitted")
  )
})

test_that("epred_draws.bartmodel warns when newdata is supplied with include_newdata = TRUE", {
  expect_warning(
    epred_draws(fixture_stochtree, newdata = newX, value = "fitted", include_newdata = TRUE),
    "include_newdata"
  )
  expect_no_warning(
    epred_draws(fixture_stochtree, newdata = newX, value = "fitted", include_newdata = FALSE)
  )
})

test_that("epred_draws.bartmodel defaults to the response (probability) scale for a binary outcome model", {
  ed_default <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE)
  ed_prob <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "probability")

  expect_equal(ed_default, ed_prob)
  expect_true(all(ed_default$fitted >= 0 & ed_default$fitted <= 1))
})

test_that("epred_draws.bartmodel scale = 'probability' matches pnorm() of the linear (probit) scale for a binary outcome model", {
  ed_linear <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "linear")
  ed_prob <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "probability")

  comp <- dplyr::left_join(as.data.frame(ed_linear), as.data.frame(ed_prob), by = c(".row", ".draw"), suffix = c("_linear", "_prob"))
  expect_equal(comp$fitted_prob, pnorm(comp$fitted_linear))
  expect_true(all(comp$fitted_prob >= 0 & comp$fitted_prob <= 1))
})

test_that("epred_draws.bartmodel scale accepts unambiguous abbreviations ('lin', 'prob')", {
  ed_lin_abbrev <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "lin")
  ed_linear <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "linear")
  expect_equal(ed_lin_abbrev, ed_linear)

  ed_prob_abbrev <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "prob")
  ed_probability <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "fitted", include_newdata = FALSE, scale = "probability")
  expect_equal(ed_prob_abbrev, ed_probability)
})

test_that("epred_draws.bartmodel include_sigsqs = TRUE adds sigsq = model$sigma2_global_samples", {
  ed <- epred_draws(fixture_stochtree, newdata = newX, value = "fitted", include_newdata = FALSE, include_sigsqs = TRUE)

  expect_true("sigsq" %in% names(ed))
  sigsq_by_draw <- ed %>% dplyr::ungroup() %>% dplyr::distinct(.data$.draw, .data$sigsq) %>% dplyr::arrange(.data$.draw)
  expect_equal(sigsq_by_draw$sigsq, fixture_stochtree$sigma2_global_samples)
})

test_that("epred_draws.bartmodel warns and ignores non-NULL ndraws", {
  expect_warning(
    epred_draws(fixture_stochtree, newdata = newX, ndraws = 5L, include_newdata = FALSE),
    "ndraws"
  )
})

test_that("fitted_draws_stochtree requires scale to be supplied explicitly", {
  expect_error(
    tidytreatment:::fitted_draws_stochtree(fixture_stochtree, newdata = newX, include_newdata = FALSE),
    "scale"
  )
})

test_that("epred_draws.bartmodel rejects non-bartmodel objects", {
  expect_error(epred_draws(structure(list(), class = "not_a_bartmodel")))
})

# --- linpred_draws.bartmodel ----------------------------------------------

test_that("linpred_draws.bartmodel equals epred_draws(..., scale = 'linear') (continuous outcome)", {
  ed <- epred_draws(fixture_stochtree, newdata = newX, value = "ep", include_newdata = FALSE, scale = "linear")
  lp <- linpred_draws(fixture_stochtree, newdata = newX, value = "lp", include_newdata = FALSE)

  comp <- dplyr::left_join(as.data.frame(ed), as.data.frame(lp), by = c(".row", ".draw"))
  expect_equal(comp$lp, comp$ep)
})

test_that("linpred_draws.bartmodel equals epred_draws(..., scale = 'linear') (binary/probit outcome)", {
  ed <- epred_draws(fixture_stochtree_bin, newdata = newX, value = "ep", include_newdata = FALSE, scale = "linear")
  lp <- linpred_draws(fixture_stochtree_bin, newdata = newX, value = "lp", include_newdata = FALSE)

  comp <- dplyr::left_join(as.data.frame(ed), as.data.frame(lp), by = c(".row", ".draw"))
  expect_equal(comp$lp, comp$ep)
  # linpred should NOT be probability-bounded (unlike epred on the probability scale)
  expect_true(any(lp$lp < 0 | lp$lp > 1))
})

# --- predicted_draws.bartmodel ---------------------------------------------

test_that("predicted_draws.bartmodel (continuous) centers on the fitted value with plausible dispersion", {
  pd <- predicted_draws(fixture_stochtree, newdata = newX, value = "pred", include_newdata = FALSE, include_fitted = TRUE, include_sigsqs = TRUE)

  expect_true(all(c("pred", ".fit", "sigsq") %in% names(pd)))

  resid <- pd$pred - pd$.fit
  expect_equal(mean(resid), 0, tolerance = 0.5)
  # empirical sd of the noise should be in the right ballpark vs sqrt(sigsq)
  expect_equal(sd(resid), mean(sqrt(pd$sigsq)), tolerance = 1)
})

test_that("predicted_draws.bartmodel (continuous) drops .fit/sigsq by default", {
  pd <- predicted_draws(fixture_stochtree, newdata = newX, value = "pred", include_newdata = FALSE)

  expect_false(any(c(".fit", "sigsq") %in% names(pd)))
  expect_true("pred" %in% names(pd))
})

test_that("predicted_draws.bartmodel (binary) draws 0/1 outcomes consistent with the fitted probability", {
  pd <- predicted_draws(fixture_stochtree_bin, newdata = newX, value = "pred", include_newdata = FALSE, include_fitted = TRUE)

  expect_true(all(pd$pred %in% c(0L, 1L)))
  expect_true(all(pd$.fitted >= 0 & pd$.fitted <= 1))

  row_summary <- pd %>%
    dplyr::group_by(.data$.row) %>%
    dplyr::summarise(emp_mean = mean(.data$pred), fitted_prob = mean(.data$.fitted), .groups = "drop")
  expect_true(all(abs(row_summary$emp_mean - row_summary$fitted_prob) < 0.5))
})

# --- residual_draws.bartmodel ----------------------------------------------

test_that("residual_draws.bartmodel = response - fitted (real/latent scale)", {
  rd <- residual_draws(fixture_stochtree, newdata = newX, response = fixture_stochtree_y[1:5], value = "resid", include_newdata = FALSE)
  ed <- epred_draws(fixture_stochtree, newdata = newX, value = "fitted", include_newdata = FALSE)

  comp <- dplyr::left_join(as.data.frame(rd), as.data.frame(ed), by = c(".row", ".draw"))
  expect_equal(comp$resid, fixture_stochtree_y[1:5][comp$.row] - comp$fitted)
})

test_that("residual_draws.bartmodel requires a response argument", {
  expect_error(
    residual_draws(fixture_stochtree, newdata = newX),
    "response"
  )
})

# --- tidy_draws.bartmodel ---------------------------------------------------

test_that("tidy_draws.bartmodel returns per-draw sigma2_global and sigma2_leaf", {
  td <- tidy_draws(fixture_stochtree)

  expect_true(all(c(".chain", ".iteration", ".draw", "sigma2_global", "sigma2_leaf") %in% names(td)))
  expect_equal(nrow(td), length(fixture_stochtree$sigma2_global_samples))
  expect_equal(td$sigma2_global, fixture_stochtree$sigma2_global_samples)
  expect_equal(td$sigma2_leaf, fixture_stochtree$sigma2_leaf_samples)
  expect_true(all(is.na(td$.chain)))
})
