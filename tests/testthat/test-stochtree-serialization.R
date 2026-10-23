skip_if_not_installed("stochtree")

# stochtree model objects hold external C++ pointers, so the only
# session-to-session-safe way to persist one is a save-to-JSON/reload round
# trip (see ?stochtree::BARTSerialization / ?stochtree::BCFSerialization).
# This file checks that every tidytreatment method for `bartmodel`/
# `bcfmodel` still behaves correctly on a *reloaded* model, not just a
# freshly-fit one - reloading is a real, intentional use case (the whole
# point of the serialization functions), not an edge case.
#
# Two things were confirmed empirically (not assumed) before writing these:
#   1. model_params fields, forest objects, and random-effects samples all
#      survive the round trip intact - tidy_draws()/variance_draws()/
#      covariate_importance() give identical results before and after.
#   2. Cached *in-sample* training predictions (y_hat_train etc.) do NOT
#      survive the round trip - stochtree itself raises a clear, specific
#      error ("This model does not have in-sample mean function prediction
#      samples" / "...treatment effect forest predictions") rather than
#      silently returning something wrong. This isn't a tidytreatment bug to
#      fix - there's nothing to recover it from, since the reloaded object
#      doesn't retain X_train either. The correct, verified workaround is to
#      pass the original training data back in as `newdata` (plus
#      `rfx_group_ids`/`treatment`/`propensity` as applicable), which gives
#      results identical to the pre-reload in-sample call.

reload_bartmodel <- function(model) stochtree::createBARTModelFromJson(stochtree::saveBARTModelToJson(model))
reload_bcfmodel <- function(model) stochtree::createBCFModelFromJson(stochtree::saveBCFModelToJson(model))

# Compares only the deterministic columns of a draws tibble - excludes any
# column added by predicted_draws()'s own stats::rnorm()/rbinom() call,
# which is never reproducible across two independent calls (no shared seed).
expect_draws_equal <- function(a, b, cols) {
  expect_equal(as.data.frame(a)[, cols], as.data.frame(b)[, cols])
}

# ============================= stochtree::bart ==============================

test_that("tidy_draws.bartmodel survives a save-to-JSON/reload round trip", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  expect_equal(tidy_draws(fixture_stochtree), tidy_draws(reloaded))
})

test_that("variance_draws.bartmodel survives a save-to-JSON/reload round trip", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  expect_equal(variance_draws(fixture_stochtree), variance_draws(reloaded))
})

test_that("covariate_importance.bartmodel survives a save-to-JSON/reload round trip", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  expect_equal(
    covariate_importance(fixture_stochtree, X_train = fixture_stochtree_x),
    covariate_importance(reloaded, X_train = fixture_stochtree_x)
  )
})

test_that("epred_draws.bartmodel (in-sample, no newdata) errors clearly after a reload instead of silently returning something wrong", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  expect_error(epred_draws(reloaded, value = "v"), "in-sample")
})

test_that("epred_draws.bartmodel with newdata = the original training X matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  before <- epred_draws(fixture_stochtree, value = "v")
  after <- epred_draws(reloaded, newdata = fixture_stochtree_x, value = "v")
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("linpred_draws.bartmodel with newdata matches before/after a reload", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  before <- linpred_draws(fixture_stochtree, newdata = fixture_stochtree_x, value = "v")
  after <- linpred_draws(reloaded, newdata = fixture_stochtree_x, value = "v")
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("residual_draws.bartmodel (in-sample, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  expect_error(residual_draws(reloaded, response = fixture_stochtree_y, value = "v"), "in-sample")
})

test_that("residual_draws.bartmodel with newdata matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  before <- residual_draws(fixture_stochtree, response = fixture_stochtree_y, value = "v")
  after <- residual_draws(reloaded, newdata = fixture_stochtree_x, response = fixture_stochtree_y, value = "v")
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("predicted_draws.bartmodel (in-sample, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  expect_error(predicted_draws(reloaded, value = "v"), "in-sample")
})

test_that("predicted_draws.bartmodel with newdata works after a reload (the .fit column it's built on is deterministic and checked; .prediction itself is a fresh random draw each call, not compared)", {
  skip_if(is.null(fixture_stochtree))
  reloaded <- reload_bartmodel(fixture_stochtree)
  before <- predicted_draws(fixture_stochtree, value = "v", include_fitted = TRUE)
  after <- predicted_draws(reloaded, newdata = fixture_stochtree_x, value = "v", include_fitted = TRUE)
  expect_draws_equal(before, after, c(".row", ".draw", ".fit"))
  expect_false(anyNA(after$v))
})

# --------------------------- stochtree::bart + rfx ---------------------------

test_that("epred_draws.bartmodel (in-sample, rfx, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_stochtree_rfx))
  reloaded <- reload_bartmodel(fixture_stochtree_rfx)
  expect_error(epred_draws(reloaded, value = "v"), "in-sample")
})

test_that("epred_draws.bartmodel with newdata + rfx_group_ids matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_stochtree_rfx))
  reloaded <- reload_bartmodel(fixture_stochtree_rfx)
  before <- epred_draws(fixture_stochtree_rfx, value = "v")
  after <- epred_draws(
    reloaded, newdata = fixture_stochtree_rfx_x, rfx_group_ids = fixture_stochtree_rfx_group, value = "v"
  )
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("residual_draws.bartmodel with newdata + rfx_group_ids matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_stochtree_rfx))
  reloaded <- reload_bartmodel(fixture_stochtree_rfx)
  before <- residual_draws(fixture_stochtree_rfx, response = fixture_stochtree_rfx_y, value = "v")
  after <- residual_draws(
    reloaded, newdata = fixture_stochtree_rfx_x, rfx_group_ids = fixture_stochtree_rfx_group,
    response = fixture_stochtree_rfx_y, value = "v"
  )
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

# ============================== stochtree::bcf ===============================

test_that("tidy_draws.bcfmodel survives a save-to-JSON/reload round trip", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_equal(tidy_draws(fixture_bcf), tidy_draws(reloaded))
})

test_that("variance_draws.bcfmodel survives a save-to-JSON/reload round trip", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_equal(variance_draws(fixture_bcf), variance_draws(reloaded))
})

test_that("covariate_importance.bcfmodel (both forests) survives a save-to-JSON/reload round trip", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_equal(
    covariate_importance(fixture_bcf, X_train = fixture_bcf_x, forest = "prognostic"),
    covariate_importance(reloaded, X_train = fixture_bcf_x, forest = "prognostic")
  )
  expect_equal(
    covariate_importance(fixture_bcf, X_train = fixture_bcf_x, forest = "treatment"),
    covariate_importance(reloaded, X_train = fixture_bcf_x, forest = "treatment")
  )
})

test_that("epred_draws.bcfmodel (in-sample, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_error(epred_draws(reloaded, value = "v"), "in-sample")
})

test_that("epred_draws.bcfmodel with newdata = the original training data matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  before <- epred_draws(fixture_bcf, value = "v")
  after <- epred_draws(
    reloaded, newdata = fixture_bcf_x, treatment = fixture_bcf_z, propensity = fixture_bcf_pi, value = "v"
  )
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("residual_draws.bcfmodel (in-sample, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_error(residual_draws(reloaded, response = fixture_bcf_y, value = "v"), "in-sample")
})

test_that("residual_draws.bcfmodel with newdata matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  before <- residual_draws(fixture_bcf, response = fixture_bcf_y, value = "v")
  after <- residual_draws(
    reloaded, newdata = fixture_bcf_x, treatment = fixture_bcf_z, propensity = fixture_bcf_pi,
    response = fixture_bcf_y, value = "v"
  )
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("predicted_draws.bcfmodel (in-sample, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_error(predicted_draws(reloaded, value = "v"), "in-sample")
})

test_that("predicted_draws.bcfmodel with newdata works after a reload", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  before <- predicted_draws(fixture_bcf, value = "v", include_fitted = TRUE)
  after <- predicted_draws(
    reloaded, newdata = fixture_bcf_x, treatment = fixture_bcf_z, propensity = fixture_bcf_pi,
    value = "v", include_fitted = TRUE
  )
  expect_draws_equal(before, after, c(".row", ".draw", ".fit"))
  expect_false(anyNA(after$v))
})

test_that("treatment_effects.bcfmodel (in-sample, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  expect_error(treatment_effects(reloaded), "in-sample")
})

test_that("treatment_effects.bcfmodel with newdata matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_bcf))
  reloaded <- reload_bcfmodel(fixture_bcf)
  before <- treatment_effects(fixture_bcf)
  after <- treatment_effects(reloaded, newdata = fixture_bcf_x, treatment = fixture_bcf_z, propensity = fixture_bcf_pi)
  expect_draws_equal(before, after, c(".row", ".draw", "cte"))
})

# ---------------------------- stochtree::bcf + rfx ---------------------------

test_that("epred_draws.bcfmodel (in-sample, rfx, no newdata) errors clearly after a reload", {
  skip_if(is.null(fixture_bcf_rfx_intercept))
  reloaded <- reload_bcfmodel(fixture_bcf_rfx_intercept)
  expect_error(epred_draws(reloaded, value = "v"), "in-sample")
})

test_that("epred_draws.bcfmodel with newdata + rfx_group_ids matches the pre-reload in-sample result exactly", {
  skip_if(is.null(fixture_bcf_rfx_intercept))
  reloaded <- reload_bcfmodel(fixture_bcf_rfx_intercept)
  before <- epred_draws(fixture_bcf_rfx_intercept, value = "v")
  after <- epred_draws(
    reloaded, newdata = fixture_bcf_rfx_x, treatment = fixture_bcf_rfx_z, propensity = fixture_bcf_rfx_pi,
    rfx_group_ids = fixture_bcf_rfx_group, value = "v"
  )
  expect_draws_equal(before, after, c(".row", ".draw", "v"))
})

test_that("treatment_effects.bcfmodel with newdata + rfx_group_ids matches the pre-reload in-sample result exactly (model_spec = 'intercept_only', so rfx doesn't touch tau itself)", {
  skip_if(is.null(fixture_bcf_rfx_intercept))
  reloaded <- reload_bcfmodel(fixture_bcf_rfx_intercept)
  before <- treatment_effects(fixture_bcf_rfx_intercept)
  after <- treatment_effects(
    reloaded, newdata = fixture_bcf_rfx_x, treatment = fixture_bcf_rfx_z, propensity = fixture_bcf_rfx_pi,
    rfx_group_ids = fixture_bcf_rfx_group
  )
  expect_draws_equal(before, after, c(".row", ".draw", "cte"))
})

test_that("treatment_effects.bcfmodel ('intercept_plus_treatment' rfx) requires rfx_group_ids with newdata after a reload, and matches the pre-reload in-sample result when supplied", {
  skip_if(is.null(fixture_bcf_rfx_ipt))
  reloaded <- reload_bcfmodel(fixture_bcf_rfx_ipt)

  # Explicit newdata/treatment/rfx_group_ids required here even before any
  # reload, since this model_spec folds the random effect into tau itself -
  # see treatment_effects.bcfmodel()'s own documentation.
  before <- treatment_effects(
    fixture_bcf_rfx_ipt, newdata = fixture_bcf_rfx_x, treatment = fixture_bcf_rfx_z,
    propensity = fixture_bcf_rfx_pi, rfx_group_ids = fixture_bcf_rfx_group
  )
  after <- treatment_effects(
    reloaded, newdata = fixture_bcf_rfx_x, treatment = fixture_bcf_rfx_z,
    propensity = fixture_bcf_rfx_pi, rfx_group_ids = fixture_bcf_rfx_group
  )
  expect_draws_equal(before, after, c(".row", ".draw", "cte"))
})
