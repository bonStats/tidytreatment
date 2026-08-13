library(BART)
library(dplyr)

md_z1 <- md_z0 <- bartmodel1_modelmatrix
md_z1[, "z"] <- 1
md_z0[, "z"] <- 0
check_matrix <- predict(bartmodel1, newdata = md_z1) - predict(bartmodel1, newdata = md_z0)

test_that("avg_treatment_effects (subset = 'all') matches manual ATE computation", {
  ate_a <- avg_treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data) %>%
    arrange(.draw)

  expect_equal(ate_a$ate, rowMeans(check_matrix))
})

test_that("avg_treatment_effects (subset = 'treated') matches manual ATT computation", {
  att_a <- avg_treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data, subset = "treated") %>%
    arrange(.draw)

  expect_equal(att_a$ate, rowMeans(check_matrix[, bartmodel1_modelmatrix[, "z"] == 1]))
})

test_that("avg_treatment_effects (subset = 'nontreated') averages over untreated rows only", {
  atnt <- avg_treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data, subset = "nontreated") %>%
    arrange(.draw)

  expect_equal(atnt$ate, rowMeans(check_matrix[, bartmodel1_modelmatrix[, "z"] == 0]))
})

test_that("avg_treatment_effects agrees with tidy_ate when using the original data as newdata", {
  a1 <- avg_treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data) %>% arrange(.draw)
  a2 <- tidy_ate(bartmodel1, treatment = "z", newdata = suhillsim1$data) %>% arrange(.draw)

  expect_equal(a1$ate, a2$ate)
})

test_that("avg_treatment_effects errors on an invalid subset value", {
  expect_error(
    avg_treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data, subset = "bogus")
  )
})
