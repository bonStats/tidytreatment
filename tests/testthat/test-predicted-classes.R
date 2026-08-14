library(BART)
library(dplyr)

skip_if_not_installed("BART")
skip_if(is.null(fixture_pbart))
skip_if(is.null(fixture_lbart))

check_bernoulli_predicted_draws <- function(model) {
  # predicted_draws.pbart/.lbart always retain the intermediate `.fitted`
  # (probability-scale) column alongside the requested `value` column.
  pred_out <- predicted_draws(model, newdata = fixture_bin_x, value = "pred", include_newdata = FALSE)

  expect_true(all(c("pred", ".fitted") %in% names(pred_out)))
  expect_true(all(pred_out$pred %in% c(0L, 1L)))
  expect_true(all(pred_out$.fitted >= 0 & pred_out$.fitted <= 1))

  # empirical draw frequency per row should track the fitted probability
  # (20 draws/row; allow a generous tolerance to avoid flaky failures)
  row_summary <- pred_out %>%
    dplyr::group_by(.data$.row) %>%
    dplyr::summarise(emp_mean = mean(.data$pred), fitted_prob = mean(.data$.fitted), .groups = "drop")

  expect_true(all(abs(row_summary$emp_mean - row_summary$fitted_prob) < 0.5))
}

test_that("predicted_draws.pbart draws Bernoulli(prob) consistent with the fitted probability scale", {
  check_bernoulli_predicted_draws(fixture_pbart)
})

test_that("predicted_draws.lbart draws Bernoulli(prob) consistent with the fitted probability scale", {
  check_bernoulli_predicted_draws(fixture_lbart)
})

test_that("predicted_draws.pbart warns and ignores non-NULL ndraws", {
  expect_warning(
    predicted_draws(fixture_pbart, newdata = fixture_bin_x, ndraws = 5L),
    "ndraws"
  )
})

test_that("predicted_draws.pbart output has one row per row/draw combination", {
  pred_out <- predicted_draws(fixture_pbart, newdata = fixture_bin_x, value = "pred")

  expect_equal(nrow(pred_out), nrow(fixture_bin_x) * nrow(fixture_pbart$yhat.train))
  expect_equal(sort(unique(pred_out$.row)), 1:nrow(fixture_bin_x))
})

test_that("predicted_draws.pbart/.lbart do not attach newdata by default, but do when include_newdata = TRUE", {
  pred_default <- predicted_draws(fixture_pbart, newdata = fixture_bin_x, value = "pred")
  expect_false(any(colnames(fixture_bin_x) %in% names(pred_default)))

  pred_attached <- predicted_draws(fixture_pbart, newdata = fixture_bin_x, value = "pred", include_newdata = TRUE)
  expect_true(all(colnames(fixture_bin_x) %in% names(pred_attached)))

  pred_attached_l <- predicted_draws(fixture_lbart, newdata = fixture_bin_x, value = "pred", include_newdata = TRUE)
  expect_true(all(colnames(fixture_bin_x) %in% names(pred_attached_l)))
})
