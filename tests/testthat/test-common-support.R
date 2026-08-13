library(BART)
library(dplyr)

fitted_and_cf <- tidytreatment:::fitted_with_counter_factual_draws(
  bartmodel1,
  newdata = suhillsim1$data, treatment = "z", subset = "all"
)

sd_summary <- fitted_and_cf %>%
  dplyr::summarise(sd_observed = sd(observed), sd_cfactual = sd(cfactual))

test_that("has_common_support method = 'sd' matches Hill & Su decision rule", {
  sd_obs_treated <- sd_summary$sd_observed[suhillsim1$data$z == 1]
  m_a <- max(sd_obs_treated)
  expected <- sd_summary$sd_cfactual < m_a + sd(sd_obs_treated)

  res <- has_common_support(bartmodel1, treatment = "z", method = "sd", modeldata = suhillsim1$data)

  expect_equal(res$common_support, expected)
})

test_that("has_common_support method = 'chisq' matches chi-squared decision rule", {
  cutoff <- 0.1
  expected <- (sd_summary$sd_cfactual / sd_summary$sd_observed)^2 < stats::qchisq(1 - cutoff, 1)

  res <- has_common_support(bartmodel1, treatment = "z", method = "chisq", cutoff = cutoff, modeldata = suhillsim1$data)

  expect_equal(res$common_support, expected)
})

test_that("has_common_support method = 'chisq' requires a cutoff", {
  expect_error(
    has_common_support(bartmodel1, treatment = "z", method = "chisq", modeldata = suhillsim1$data)
  )
})

test_that("has_common_support method = 'sd' warns if cutoff is supplied (and ignores it)", {
  expect_warning(
    res <- has_common_support(bartmodel1, treatment = "z", method = "sd", cutoff = 0.5, modeldata = suhillsim1$data),
    "cutoff"
  )

  expected <- has_common_support(bartmodel1, treatment = "z", method = "sd", modeldata = suhillsim1$data)
  expect_equal(res$common_support, expected$common_support)
})

test_that("has_common_support warns and returns NA for an unrecognised method", {
  expect_warning(
    res <- has_common_support(bartmodel1, treatment = "z", method = "not-a-method", modeldata = suhillsim1$data),
    "specify"
  )

  expect_true(all(is.na(res$common_support)))
})

test_that("has_common_support errors when treatment column is not 0/1 integer or logical", {
  bad_data <- suhillsim1$data
  bad_data$z <- as.numeric(bad_data$z)

  expect_error(
    has_common_support(bartmodel1, treatment = "z", method = "sd", modeldata = bad_data)
  )
})

test_that("has_common_support errors when treatment is not a column of modeldata", {
  expect_error(
    has_common_support(bartmodel1, treatment = "not_a_column", method = "sd", modeldata = suhillsim1$data)
  )
})

test_that("treatment_effects with common_support_method adds a common_support column", {
  te <- treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data, common_support_method = "sd")

  expect_true("common_support" %in% names(te))
  expect_type(te$common_support, "logical")
})

test_that("treatment_effects messages when newdata is supplied alongside common_support_method", {
  expect_message(
    treatment_effects(bartmodel1, treatment = "z", newdata = suhillsim1$data, common_support_method = "sd"),
    "original dataset"
  )
})

test_that("has_tidytreatment_methods is FALSE for wbart (no model.matrix method registered)", {
  expect_false(has_tidytreatment_methods(bartmodel1))
})
