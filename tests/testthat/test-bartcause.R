library(dplyr)

skip_if_not_installed("bartCause")
skip_if_not_installed("lme4")
skip_if(is.null(fixture_bartc))

# fit.rsp's formula includes the propensity score ("ps") as a covariate
# (bartc() adds it internally as p.scoreAsCovariate defaults to TRUE), so
# "new" response-stage data needs a ps column too - reuse the model's own
# fitted p.score for the rows we're predicting on. The assignment-stage
# model (z ~ confounders) has no such requirement.
newdat_trt <- fixture_bartc_data[1:5, ]
newdat_rsp <- fixture_bartc_data[1:5, ]
newdat_rsp$ps <- fixture_bartc$p.score[1:5]

test_that("epred_draws.bartcFit delegates to the response/assignment sub-model", {
  ed_rsp <- epred_draws(fixture_bartc, value = "fitted")
  ed_rsp_expected <- tidybayes::epred_draws(fixture_bartc$fit.rsp, value = "fitted")
  expect_equal(ed_rsp, ed_rsp_expected)

  ed_trt <- epred_draws(fixture_bartc, value = "fitted", fitstage = "assignment")
  ed_trt_expected <- tidybayes::epred_draws(fixture_bartc$fit.trt, value = "fitted")
  expect_equal(ed_trt, ed_trt_expected)
})

test_that("epred_draws.bartcFit delegates to the response/assignment sub-model with newdata", {
  ed_rsp <- suppressWarnings(epred_draws(fixture_bartc, newdata = newdat_rsp, value = "fitted", include_newdata = FALSE))
  ed_rsp_expected <- suppressWarnings(tidybayes::epred_draws(fixture_bartc$fit.rsp, newdata = newdat_rsp, value = "fitted", include_newdata = FALSE))
  expect_equal(ed_rsp, ed_rsp_expected)
  expect_equal(sort(unique(ed_rsp$.row)), 1:5)

  ed_trt <- suppressWarnings(epred_draws(fixture_bartc, newdata = newdat_trt, value = "fitted", fitstage = "assignment", include_newdata = FALSE))
  ed_trt_expected <- suppressWarnings(tidybayes::epred_draws(fixture_bartc$fit.trt, newdata = newdat_trt, value = "fitted", include_newdata = FALSE))
  expect_equal(ed_trt, ed_trt_expected)
  expect_equal(sort(unique(ed_trt$.row)), 1:5)
})

test_that("predicted_draws.bartcFit delegates to the response/assignment sub-model", {
  pd_rsp <- predicted_draws(fixture_bartc, value = "pred")
  expect_equal(nrow(pd_rsp), nrow(fixture_bartc_data) * length(unique(pd_rsp$.draw)))
})

test_that("predicted_draws.bartcFit works with newdata", {
  pd_rsp <- suppressWarnings(predicted_draws(fixture_bartc, newdata = newdat_rsp, value = "pred", include_newdata = FALSE))
  expect_equal(sort(unique(pd_rsp$.row)), 1:5)
  expect_true(is.numeric(pd_rsp$pred))
})

test_that("linpred_draws.bartcFit delegates to the response/assignment sub-model", {
  lp_rsp <- linpred_draws(fixture_bartc, value = "lp")
  lp_rsp_expected <- tidybayes::linpred_draws(fixture_bartc$fit.rsp, value = "lp")
  expect_equal(lp_rsp, lp_rsp_expected)
})

test_that("linpred_draws.bartcFit delegates to the response sub-model with newdata", {
  lp_rsp <- suppressWarnings(linpred_draws(fixture_bartc, newdata = newdat_rsp, value = "lp", include_newdata = FALSE))
  lp_rsp_expected <- suppressWarnings(tidybayes::linpred_draws(fixture_bartc$fit.rsp, newdata = newdat_rsp, value = "lp", include_newdata = FALSE))
  expect_equal(lp_rsp, lp_rsp_expected)
  expect_equal(sort(unique(lp_rsp$.row)), 1:5)
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

test_that("treatment_effects.bartcFit returns per-row/draw cte (bartCause's icate), ite and treatment status", {
  te <- treatment_effects(fixture_bartc)

  expect_true(all(c("cte", "ite", "treated") %in% names(te)))
  expect_false("icate" %in% names(te))

  # cte is bartCause's icate (mu_hat(1) - mu_hat(0)) under the package-wide
  # "cte" column name used by treatment_effects.default/.bcfmodel and relied
  # on by avg_treatment_effects()/tidy_ate()/tidy_att().
  expect_equal(mean(te$cte), mean(bartCause::extract(fixture_bartc, type = "icate")))

  te_t <- treatment_effects(fixture_bartc, subset = "treated")
  te_nt <- treatment_effects(fixture_bartc, subset = "nontreated")

  expect_equal(nrow(te_t) + nrow(te_nt), nrow(te))
  expect_true(all(te_t$treated == 1))
  expect_true(all(te_nt$treated == 0))
})

test_that("avg_treatment_effects works end-to-end with a bartcFit model", {
  ate <- avg_treatment_effects(fixture_bartc)

  expect_true(all(c(".chain", ".iteration", ".draw", "ate") %in% names(ate)))
  expect_equal(nrow(ate), length(unique(tidy_draws(fixture_bartc)$.draw)))

  te <- treatment_effects(fixture_bartc)
  te_means <- te %>%
    dplyr::group_by(.data$.chain, .data$.iteration, .data$.draw) %>%
    dplyr::summarise(ate_check = mean(.data$cte), .groups = "drop")

  comp <- dplyr::left_join(ate, te_means, by = c(".chain", ".iteration", ".draw"))
  expect_equal(comp$ate, comp$ate_check)

  att <- avg_treatment_effects(fixture_bartc, subset = "treated")
  expect_true("ate" %in% names(att))
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
