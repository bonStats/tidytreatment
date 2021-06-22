# context("Residuals") deprecated

library(BART)
library(dplyr)
library(tidyr)

# rows = MCMC samples, cols = observations
smpls <- nrow(bartmodel1$yhat.train)
check_matrix <- matrix(rep(suhillsim1$data$y, smpls), nrow = smpls, byrow = TRUE) - bartmodel1$yhat.train
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

test_that("Residual values calculated correctly", {
  td_fd <- residual_draws(bartmodel1, newdata = suhillsim1$data, response = suhillsim1$data$y, include_newdata = FALSE, residual = "resid")
  comp_df <- td_fd %>% full_join(check_df, by = c(".row", ".draw"))
  expect_equal(comp_df$resid, comp_df$resid_check)
})
