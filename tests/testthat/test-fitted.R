# context("Fitted") deprecated

library(BART)
library(dplyr)
library(tidyr)

# rows = MCMC samples, cols = observations
check_matrix <- bartmodel1$yhat.train
colnames(check_matrix) <- 1:ncol(check_matrix)
check_df <- check_matrix %>%
  as_tibble() %>%
  mutate(.draw = 1:n()) %>%
  pivot_longer(
    cols = all_of(1:ncol(check_matrix)),
    names_to = ".row",
    values_to = "fitted_check"
  ) %>%
  mutate(.row = as.integer(.row))

test_that("Fitted values calculated correctly", {
  td_fd <- epred_draws(bartmodel1, newdata = suhillsim1$data, include_newdata = FALSE, value = "fitted")
  comp_df <- td_fd %>% full_join(check_df, by = c(".row", ".draw"))
  expect_equal(comp_df$fitted, comp_df$fitted_check)
})
