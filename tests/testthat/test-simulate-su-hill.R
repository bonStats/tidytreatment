test_that("su_hill_true_effects() exactly reconstructs simulate_su_hill_data()'s mean_y (response_parallel = TRUE)", {
  withr::with_seed(42, {
    sim <- simulate_su_hill_data(n = 500, tau = 4, response_parallel = TRUE)
  })

  truth <- su_hill_true_effects(sim)
  z <- sim$data$z

  expect_equal(truth$mu0[z == 0], sim$mean_y[z == 0])
  expect_equal(truth$mu1[z == 1], sim$mean_y[z == 1])
  expect_equal(truth$ate, 4)
  expect_equal(truth$tau, 4)
  expect_equal(stats::sd(truth$ite), 0)
})

test_that("su_hill_true_effects() exactly reconstructs simulate_su_hill_data()'s mean_y (response_parallel = FALSE)", {
  withr::with_seed(42, {
    sim <- simulate_su_hill_data(n = 500, tau = 4, response_parallel = FALSE)
  })

  truth <- su_hill_true_effects(sim)
  z <- sim$data$z

  expect_equal(truth$mu0[z == 0], sim$mean_y[z == 0])
  expect_equal(truth$mu1[z == 1], sim$mean_y[z == 1])
  expect_true(stats::sd(truth$ite) > 0)
})

test_that("su_hill_true_effects() exactly reconstructs simulate_su_hill_data()'s mean_y (add_categorical = TRUE)", {
  withr::with_seed(1, {
    sim <- simulate_su_hill_data(
      n = 500, tau = 4, add_categorical = TRUE,
      coef_categorical_treatment = c(0, 0, 1), coef_categorical_nontreatment = c(-1, 0, -1)
    )
  })

  truth <- su_hill_true_effects(sim)
  z <- sim$data$z

  expect_equal(truth$mu0[z == 0], sim$mean_y[z == 0])
  expect_equal(truth$mu1[z == 1], sim$mean_y[z == 1])
})

test_that("su_hill_true_effects() requires a suhillsim object", {
  expect_error(su_hill_true_effects(list()))
})

test_that("su_hill_true_effects() errors on an object with no ground_truth attribute", {
  sim <- structure(list(), class = "suhillsim")
  expect_error(su_hill_true_effects(sim), "ground_truth")
})
