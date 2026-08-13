library(BART)
library(dplyr)
library(tidyr)

skip_if_not_installed("BART")
skip_if(is.null(fixture_pbart))

test_that("residual_draws.pbart = response - fitted (real/latent scale)", {
  check_matrix <- fixture_pbart$yhat.train
  smpls <- nrow(check_matrix)
  y_matrix <- matrix(rep(fixture_bin_y, smpls), nrow = smpls, byrow = TRUE)
  check_matrix <- y_matrix - check_matrix

  colnames(check_matrix) <- 1:ncol(check_matrix)
  check_df <- check_matrix %>%
    as_tibble() %>%
    mutate(.draw = 1:n()) %>%
    pivot_longer(
      cols = all_of(1:ncol(check_matrix)),
      names_to = ".row",
      values_to = "resid_check"
    ) %>%
    mutate(.row = as.integer(.row))

  td_rd <- residual_draws(fixture_pbart, newdata = fixture_bin_x, response = fixture_bin_y, include_newdata = FALSE, value = "resid")
  comp_df <- td_rd %>% full_join(check_df, by = c(".row", ".draw"))

  expect_equal(comp_df$resid, comp_df$resid_check)
})

test_that("residual_draws requires a response argument for BART-package models", {
  expect_error(
    residual_draws(fixture_pbart, newdata = fixture_bin_x),
    "response"
  )
})

test_that("residual_draws.pbart output is grouped by .row", {
  td_rd <- residual_draws(fixture_pbart, newdata = fixture_bin_x, response = fixture_bin_y)
  expect_true(".row" %in% dplyr::group_vars(td_rd))
})
