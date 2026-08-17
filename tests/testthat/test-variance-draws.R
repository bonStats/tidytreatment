library(BART)

test_that("variance_draws.wbart returns sigma^2 with the right shape", {
  vd <- variance_draws(bartmodel1)

  expect_equal(vd$.sigma_sq, bartmodel1$sigma^2)
  expect_equal(vd$.draw, seq_along(bartmodel1$sigma))
  # bartmodel1$sigma is a plain vector, not a matrix - one chain
  expect_true(all(vd$.chain == 1L))
  expect_equal(vd$.iteration, seq_along(bartmodel1$sigma))
})

test_that("variance_draws.wbart() recovers real .chain/.iteration for an mc.wbart() multi-chain model (regression test)", {
  skip_on_os("windows") # mc.wbart() uses parallel::mcparallel(), fork-based, unavailable on Windows
  skip_if_not_installed("BART")

  set.seed(1)
  n <- 20
  x <- data.frame(x1 = rnorm(n))
  y <- x$x1 + rnorm(n)
  fit <- BART::mc.wbart(x.train = x, y.train = y, ntree = 5L, ndpost = 9L, nskip = 3L, mc.cores = 3L, printevery = 100000L)
  expect_true(is.matrix(fit$sigma))

  vd <- variance_draws(fit)
  n_chains <- ncol(fit$sigma)
  n_per_chain <- nrow(fit$sigma)

  expect_equal(vd$.sigma_sq, as.vector(fit$sigma)^2)
  expect_equal(vd$.chain, rep(seq_len(n_chains), each = n_per_chain))
  expect_equal(vd$.iteration, rep(seq_len(n_per_chain), times = n_chains))
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
