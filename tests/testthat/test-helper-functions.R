test_that("is_01_integer_vector only accepts integer vectors of 0s and 1s", {
  expect_true(tidytreatment:::is_01_integer_vector(c(0L, 1L, 1L, 0L)))
  expect_false(tidytreatment:::is_01_integer_vector(c(0, 1, 1, 0))) # double, not integer
  expect_false(tidytreatment:::is_01_integer_vector(c(0L, 1L, 2L)))
  expect_false(tidytreatment:::is_01_integer_vector(c(TRUE, FALSE)))
})

test_that("has_installed_package detects installed and missing packages", {
  expect_true(tidytreatment:::has_installed_package("base"))
  expect_false(tidytreatment:::has_installed_package("not-a-real-package-xyz123"))
})

test_that("has_method_str detects registered S3 methods for a class", {
  expect_true(tidytreatment:::has_method_str("wbart", "epred_draws"))
  expect_true(tidytreatment:::has_method_str("wbart", "predicted_draws"))
  expect_false(tidytreatment:::has_method_str("wbart", "not_a_real_generic"))
})

test_that("check_method is silent when the method exists and errors otherwise", {
  expect_no_error(tidytreatment:::check_method(bartmodel1, "epred_draws"))

  expect_error(
    tidytreatment:::check_method(bartmodel1, "not_a_real_generic", helper = "try something else"),
    "does not have method"
  )
  expect_error(
    tidytreatment:::check_method(bartmodel1, "not_a_real_generic", helper = "try something else"),
    "try something else"
  )
})

test_that("has_tidytreatment_methods requires both epred_draws and model.matrix methods", {
  expect_false(has_tidytreatment_methods(bartmodel1)) # no model.matrix.wbart registered
})
