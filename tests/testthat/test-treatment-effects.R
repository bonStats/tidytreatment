# context("treatment effects") deprecated

library(BART)
library(dplyr)
library(tidyr)

# set up treatment effects values
md_z1 <- md_z0 <- bartmodel1_modelmatrix
md_z1[, "z"] <- 1
md_z0[, "z"] <- 0

# rows = MCMC samples, cols = observations
check_matrix <- predict(bartmodel1, newdata = md_z1) - predict(bartmodel1, newdata = md_z0)
colnames(check_matrix) <- 1:ncol(check_matrix)
check_teff_df <- check_matrix %>%
  as_tibble() %>%
  mutate(.draw = 1:n()) %>%
  pivot_longer(
    cols = all_of(1:ncol(check_matrix)),
    names_to = ".row",
    values_to = "cte_check"
  ) %>%
  mutate(.row = as.integer(.row))

test_that("Treatment effects calculated correctly", {
  td_teff <- treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data)
  comp_df <- td_teff %>% full_join(check_teff_df, by = c(".row", ".draw"))
  expect_equal(comp_df$cte, comp_df$cte_check)
})

test_that("ATE calculated correctly", {
  td_ate <- tidy_ate(bartmodel1, treatment = "z", newdata = suhillsim1$data) %>%
    arrange(.draw)
  expect_equal(td_ate$ate, rowMeans(check_matrix)) # average across obs
})

test_that("ATT calculated correctly", {
  td_att <- tidy_att(bartmodel1, treatment = "z", newdata = suhillsim1$data) %>%
    arrange(.draw)
  expect_equal(td_att$att, rowMeans(check_matrix[, bartmodel1_modelmatrix[, "z"] == 1]))
})
