library(BART)

test_that("variance_draws.wbart returns sigma^2 with the right shape", {
  vd <- variance_draws(bartmodel1)

  expect_equal(vd$.sigma_sq, bartmodel1$sigma^2)
  expect_equal(vd$.draw, seq_along(bartmodel1$sigma))
  expect_true(all(is.na(vd$.chain)))
  expect_true(all(is.na(vd$.iteration)))
})

test_that("variance_draws.wbart respects the `value` argument", {
  vd <- variance_draws(bartmodel1, value = "myvar")

  expect_true("myvar" %in% names(vd))
  expect_equal(vd$myvar, bartmodel1$sigma^2)
})

test_that("variance_draws errors for a class without a registered method", {
  x <- structure(list(), class = "not_a_supported_model")
  expect_error(variance_draws(x))
})
