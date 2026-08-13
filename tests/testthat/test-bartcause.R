library(dplyr)

skip_if_not_installed("bartCause")
skip_if_not_installed("lme4")
skip_if(is.null(fixture_bartc))

test_that("epred_draws.bartcFit delegates to the response/assignment sub-model", {
  ed_rsp <- epred_draws(fixture_bartc, value = "fitted")
  ed_rsp_expected <- tidybayes::epred_draws(fixture_bartc$fit.rsp, value = "fitted")
  expect_equal(ed_rsp, ed_rsp_expected)

  ed_trt <- epred_draws(fixture_bartc, value = "fitted", fitstage = "assignment")
  ed_trt_expected <- tidybayes::epred_draws(fixture_bartc$fit.trt, value = "fitted")
  expect_equal(ed_trt, ed_trt_expected)
})

test_that("predicted_draws.bartcFit delegates to the response/assignment sub-model", {
  pd_rsp <- predicted_draws(fixture_bartc, value = "pred")
  expect_equal(nrow(pd_rsp), nrow(fixture_bartc_data) * length(unique(pd_rsp$.draw)))
})

test_that("linpred_draws.bartcFit delegates to the response/assignment sub-model", {
  lp_rsp <- linpred_draws(fixture_bartc, value = "lp")
  lp_rsp_expected <- tidybayes::linpred_draws(fixture_bartc$fit.rsp, value = "lp")
  expect_equal(lp_rsp, lp_rsp_expected)
})

test_that("tidy_draws.bartcFit (type = NULL) delegates to the response sub-model by default", {
  td <- tidy_draws(fixture_bartc)
  td_expected <- tidybayes::tidy_draws(fixture_bartc$fit.rsp)
  expect_equal(td, td_expected)
})

test_that("tidy_draws.bartcFit (type = 'icate') warns when fitstage is left at its (length > 1) default", {
  expect_warning(
    td_icate <- tidy_draws(fixture_bartc, type = "icate"),
    "fitstage ignored"
  )
  expect_true("icate" %in% names(td_icate))
  expect_true(".row" %in% dplyr::group_vars(td_icate))
})

test_that("tidy_draws.bartcFit (type = 'icate') does not warn when fitstage is given explicitly", {
  expect_no_warning(
    tidy_draws(fixture_bartc, type = "icate", fitstage = "response")
  )
})

test_that("treatment_effects.bartcFit returns per-row/draw icate, ite and treatment status", {
  te <- treatment_effects(fixture_bartc)

  expect_true(all(c("icate", "ite", "treated") %in% names(te)))

  te_t <- treatment_effects(fixture_bartc, subset = "treated")
  te_nt <- treatment_effects(fixture_bartc, subset = "nontreated")

  expect_equal(nrow(te_t) + nrow(te_nt), nrow(te))
  expect_true(all(te_t$treated == 1))
  expect_true(all(te_nt$treated == 0))
})

test_that("treatment_effects.bartcFit rejects explicit treatment/newdata arguments", {
  expect_error(treatment_effects(fixture_bartc, treatment = "z"))
  expect_error(treatment_effects(fixture_bartc, newdata = fixture_bartc_data))
})

test_that("treatment_effects.bartcFit warns and uses a default cutoff when common_support_method is given without one", {
  expect_warning(
    te_cs <- treatment_effects(fixture_bartc, common_support_method = "sd"),
    "Default value for cutoff used"
  )
  expect_true("supported" %in% names(te_cs))
})

test_that("treatment_effects.bartcFit warns if cutoff is given without common_support_method", {
  expect_warning(
    treatment_effects(fixture_bartc, cutoff = 0.1),
    "cutoff ignored"
  )
})

test_that("covariate_importance.bartcFit dispatches on fitstage", {
  res_rsp <- covariate_importance(fixture_bartc)
  res_trt <- covariate_importance(fixture_bartc, fitstage = "assignment")

  expect_equal(res_rsp, covariate_importance(fixture_bartc$fit.rsp))
  expect_equal(res_trt, covariate_importance(fixture_bartc$fit.trt))
})
