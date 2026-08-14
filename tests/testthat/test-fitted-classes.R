library(BART)
library(dplyr)
library(tidyr)

skip_if_not_installed("BART")
skip_if(is.null(fixture_pbart))
skip_if(is.null(fixture_lbart))

check_fitted_from_matrix <- function(mat, value_col) {
  colnames(mat) <- 1:ncol(mat)
  mat %>%
    as_tibble() %>%
    mutate(.draw = 1:n()) %>%
    pivot_longer(
      cols = all_of(1:ncol(mat)),
      names_to = ".row",
      values_to = value_col
    ) %>%
    mutate(.row = as.integer(.row))
}

test_that("epred_draws.pbart defaults to the response (probability) scale", {
  check_df <- check_fitted_from_matrix(pnorm(fixture_pbart$yhat.train), "fitted_check")

  td_fd <- epred_draws(fixture_pbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted")
  comp_df <- td_fd %>% full_join(check_df, by = c(".row", ".draw"))

  expect_equal(comp_df$fitted, comp_df$fitted_check)
  expect_true(all(comp_df$fitted >= 0 & comp_df$fitted <= 1))
})

test_that("epred_draws.pbart scale = 'real' matches raw yhat.train", {
  check_df <- check_fitted_from_matrix(fixture_pbart$yhat.train, "fitted_check")

  td_fd <- epred_draws(fixture_pbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted", scale = "real")
  comp_df <- td_fd %>% full_join(check_df, by = c(".row", ".draw"))

  expect_equal(comp_df$fitted, comp_df$fitted_check)
})

test_that("epred_draws.pbart scale = 'prob' applies pnorm to real-scale draws", {
  td_real <- epred_draws(fixture_pbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted", scale = "real")
  td_prob <- epred_draws(fixture_pbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted", scale = "prob")

  comp_df <- td_real %>% full_join(td_prob, by = c(".row", ".draw"), suffix = c("_real", "_prob"))

  expect_equal(comp_df$fitted_prob, pnorm(comp_df$fitted_real))
  expect_true(all(comp_df$fitted_prob >= 0 & comp_df$fitted_prob <= 1))
})

test_that("epred_draws.lbart defaults to the response (probability) scale", {
  check_df <- check_fitted_from_matrix(plogis(fixture_lbart$yhat.train), "fitted_check")

  td_fd <- epred_draws(fixture_lbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted")
  comp_df <- td_fd %>% full_join(check_df, by = c(".row", ".draw"))

  expect_equal(comp_df$fitted, comp_df$fitted_check)
  expect_true(all(comp_df$fitted >= 0 & comp_df$fitted <= 1))
})

test_that("epred_draws.lbart scale = 'real' matches raw yhat.train", {
  check_df <- check_fitted_from_matrix(fixture_lbart$yhat.train, "fitted_check")

  td_fd <- epred_draws(fixture_lbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted", scale = "real")
  comp_df <- td_fd %>% full_join(check_df, by = c(".row", ".draw"))

  expect_equal(comp_df$fitted, comp_df$fitted_check)
})

test_that("epred_draws.lbart scale = 'prob' applies plogis to real-scale draws", {
  td_real <- epred_draws(fixture_lbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted", scale = "real")
  td_prob <- epred_draws(fixture_lbart, newdata = fixture_bin_x, include_newdata = FALSE, value = "fitted", scale = "prob")

  comp_df <- td_real %>% full_join(td_prob, by = c(".row", ".draw"), suffix = c("_real", "_prob"))

  expect_equal(comp_df$fitted_prob, plogis(comp_df$fitted_real))
})

test_that("fitted_draws_BART requires scale to be supplied explicitly", {
  expect_error(
    tidytreatment:::fitted_draws_BART(fixture_pbart, newdata = fixture_bin_x, include_newdata = FALSE),
    "scale"
  )
})

test_that("epred_draws.pbart errors when include_newdata = TRUE and newdata missing", {
  expect_error(
    epred_draws(fixture_pbart, include_newdata = TRUE),
    "newdata"
  )
})

test_that("epred_draws.pbart warns and ignores non-NULL ndraws", {
  expect_warning(
    epred_draws(fixture_pbart, newdata = fixture_bin_x, ndraws = 5L, include_newdata = FALSE),
    "ndraws"
  )
})

test_that("epred_draws grouping/columns follow tidybayes conventions", {
  td_fd <- epred_draws(fixture_pbart, newdata = fixture_bin_x, value = "fitted")

  expect_true(all(c(".row", ".chain", ".iteration", ".draw", "fitted") %in% names(td_fd)))
  expect_true(all(is.na(td_fd$.chain)))
  expect_true(all(is.na(td_fd$.iteration)))
  expect_equal(sort(unique(td_fd$.row)), 1:nrow(fixture_bin_x))
  expect_equal(sort(unique(td_fd$.draw)), 1:nrow(fixture_pbart$yhat.train))
})

test_that("multinomial BART models (mbart/mbart2) are unsupported and error informatively", {
  fake_mbart <- structure(list(), class = "mbart")
  fake_mbart2 <- structure(list(), class = "mbart2")

  expect_error(epred_draws(fake_mbart), "not supported")
  expect_error(epred_draws(fake_mbart2), "not supported")
  expect_error(predicted_draws(fake_mbart), "not supported")
  expect_error(predicted_draws(fake_mbart2), "not supported")
  expect_error(residual_draws(fake_mbart), "not supported")
  expect_error(residual_draws(fake_mbart2), "not supported")
  expect_error(covariate_importance(fake_mbart), "not supported")
  expect_error(covariate_importance(fake_mbart2), "not supported")
  expect_error(covariate_with_treatment_importance(fake_mbart, treatment = "z"), "not supported")
  expect_error(covariate_with_treatment_importance(fake_mbart2, treatment = "z"), "not supported")

  # errors regardless of whether newdata is supplied
  expect_error(epred_draws(fake_mbart, newdata = data.frame(x1 = 1)), "not supported")
})
