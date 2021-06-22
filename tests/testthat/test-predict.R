# context("Fitted") deprecated

library(BART)
library(dplyr)
library(tidyr)

smpl_id <- c(
  30L, 81L, 97L, 44L, 52L, 43L, 34L, 89L, 88L, 87L, 93L, 14L,
  68L, 17L, 8L, 45L, 85L, 66L, 94L, 35L
)

pdata <- suhillsim1$data[smpl_id, ] # randomly sample some
pdata_mm <- bartModelMatrix(pdata[, -1]) # remove "y" variable

# rows = MCMC samples, cols = observations
check_matrix <- predict(bartmodel1, newdata = pdata_mm)
colnames(check_matrix) <- 1:ncol(check_matrix)
check_df <- check_matrix %>%
  as_tibble() %>%
  mutate(.draw = 1:n()) %>%
  pivot_longer(
    cols = all_of(1:ncol(check_matrix)),
    names_to = ".row",
    values_to = "pred_check"
  ) %>%
  mutate(.row = as.integer(.row))

test_that("Predicted values calculated correctly", {
  td_pd <- predicted_draws(bartmodel1,
    newdata = pdata, include_newdata = FALSE, prediction = "pred",
    rng = function(n, mean, ...) {
      mean + 0.1
    }
  ) # random noise fixed
  comp_df <- td_pd %>% full_join(check_df, by = c(".row", ".draw"))
  expect_equal(comp_df$pred, comp_df$pred_check + 0.1)
})
