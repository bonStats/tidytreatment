# context("counter factuals") deprecated

library(BART)
library(dplyr)
library(tidyr)

# set up counter factual values
md_cf <- bartmodel1_modelmatrix
md_cf[, "z"] <- 1 - md_cf[, "z"] # 0 -> 1, 1 -> 0

# rows = MCMC samples, cols = observations
check_matrix <- predict(bartmodel1, newdata = md_cf)
colnames(check_matrix) <- 1:ncol(check_matrix)
check_df <- check_matrix %>%
  as_tibble() %>%
  mutate(.draw = 1:n()) %>%
  pivot_longer(
    cols = all_of(1:ncol(check_matrix)),
    names_to = ".row",
    values_to = "cf_check"
  ) %>%
  mutate(.row = as.integer(.row))

test_that("Counter factuals calculated correctly", {
  td_cf <- tidytreatment:::fitted_with_counter_factual_draws(bartmodel1, treatment = "z", newdata = suhillsim1$data, subset = "all")
  comp_df <- td_cf %>% full_join(check_df, by = c(".row", ".draw"))
  expect_equal(comp_df$cfactual, comp_df$cf_check)
})
