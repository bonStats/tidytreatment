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

test_that("has_common_support(scale = 'linear') differs from the (response-scale) default for a binary outcome model, and accepts abbreviations", {
  skip_if_not_installed("BART")

  withr::with_seed(42, {
    n <- 40
    x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
    z <- rbinom(n, 1, plogis(x$x1))
    y <- rbinom(n, 1, plogis(0.5 * x$x1 - 0.5 * x$x2 + z))
    dat_bin <- cbind(y = y, z = z, x)

    fit_bin <- BART::pbart(
      x.train = dplyr::select(dat_bin, -y), y.train = y,
      ndpost = 10, nskip = 5, ntree = 5, printevery = 1000L
    )
  })

  cs_default <- has_common_support(fit_bin, treatment = "z", method = "sd", modeldata = dat_bin)
  cs_linear <- has_common_support(fit_bin, treatment = "z", method = "sd", modeldata = dat_bin, scale = "linear")
  cs_prob <- has_common_support(fit_bin, treatment = "z", method = "sd", modeldata = dat_bin, scale = "probability")
  cs_lin_abbrev <- has_common_support(fit_bin, treatment = "z", method = "sd", modeldata = dat_bin, scale = "lin")

  expect_equal(cs_default, cs_prob)
  expect_equal(cs_linear, cs_lin_abbrev)
  expect_false(isTRUE(all.equal(cs_linear$sd_observed, cs_default$sd_observed)))
})
