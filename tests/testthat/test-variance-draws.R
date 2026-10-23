library(BART)

# bartmodel1 is built with nskip = 10000L, keepevery = 100L: model$sigma
# records every raw MCMC sweep (cwbart.cpp writes sdraw[i] every iteration),
# not just the kept ones, so it's length nskip + ndpost*keepevery = 30000,
# while only the last ndpost = 200 are the actual post-burn-in, kept draws
# (yhat.train's own row count). The correct comparison is always against
# this trimmed tail, not model$sigma directly - see bart_sigma_aligned()'s
# header comment in R/tidy-posterior-BART.R for the underlying mechanism.
test_that("variance_draws.wbart returns sigma^2 with the right shape, trimmed to post-burn-in draws only", {
  vd <- variance_draws(bartmodel1)
  n_total <- nrow(bartmodel1$yhat.train)
  sigma_trimmed <- utils::tail(bartmodel1$sigma, n_total)

  expect_equal(nrow(vd), n_total)
  expect_equal(vd$.sigma_sq, sigma_trimmed^2)
  expect_equal(vd$.draw, seq_len(n_total))
  # bartmodel1$sigma is a plain vector, not a matrix - one chain
  expect_true(all(vd$.chain == 1L))
  expect_equal(vd$.iteration, seq_len(n_total))
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
  # The true per-chain retained-draw count comes from yhat.train (one block
  # per chain), not from nrow(fit$sigma) directly - that includes burn-in
  # too (same issue as the single-chain case above), so using it as ground
  # truth here would just re-assert the bug rather than test against it.
  n_per_chain <- nrow(fit$yhat.train) / n_chains
  sigma_trimmed <- apply(fit$sigma, 2, utils::tail, n_per_chain)

  expect_equal(vd$.sigma_sq, as.vector(sigma_trimmed)^2)
  expect_equal(vd$.chain, rep(seq_len(n_chains), each = n_per_chain))
  expect_equal(vd$.iteration, rep(seq_len(n_per_chain), times = n_chains))
})

test_that("variance_draws.wbart respects the `value` argument", {
  vd <- variance_draws(bartmodel1, value = "myvar")
  sigma_trimmed <- utils::tail(bartmodel1$sigma, nrow(bartmodel1$yhat.train))

  expect_true("myvar" %in% names(vd))
  expect_equal(vd$myvar, sigma_trimmed^2)
})

test_that("variance_draws errors for a class without a registered method", {
  x <- structure(list(), class = "not_a_supported_model")
  expect_error(variance_draws(x))
})

test_that("variance_draws.bartmodel() (stochtree::bart) returns sigma^2 with the right shape", {
  skip_if(is.null(fixture_stochtree))

  vd <- variance_draws(fixture_stochtree)

  expect_equal(vd$.sigma_sq, fixture_stochtree$sigma2_global_samples)
  expect_equal(vd$.draw, seq_along(fixture_stochtree$sigma2_global_samples))
  expect_true(all(is.na(vd$.chain)))
  expect_true(all(is.na(vd$.iteration)))
})

test_that("variance_draws.bartmodel() respects the `value` argument", {
  skip_if(is.null(fixture_stochtree))

  vd <- variance_draws(fixture_stochtree, value = "myvar")
  expect_true("myvar" %in% names(vd))
  expect_equal(vd$myvar, fixture_stochtree$sigma2_global_samples)
})

test_that("variance_draws.bartmodel() errors for a binary (probit) outcome model", {
  skip_if(is.null(fixture_stochtree_bin))
  expect_error(variance_draws(fixture_stochtree_bin), "not applicable")
})

test_that("variance_draws.bcfmodel() (stochtree::bcf) returns sigma^2 with the right shape", {
  skip_if(is.null(fixture_bcf))

  vd <- variance_draws(fixture_bcf)

  expect_equal(vd$.sigma_sq, fixture_bcf$sigma2_global_samples)
  expect_equal(vd$.draw, seq_along(fixture_bcf$sigma2_global_samples))
  expect_true(all(is.na(vd$.chain)))
  expect_true(all(is.na(vd$.iteration)))
})

test_that("variance_draws.bcfmodel() errors for a binary (probit) outcome model", {
  skip_if(is.null(fixture_bcf_bin))
  expect_error(variance_draws(fixture_bcf_bin), "not applicable")
})

test_that("variance_draws.stan4bartFit() returns sigma^2 with the right shape", {
  skip_if(is.null(fixture_stan4bart))

  vd <- variance_draws(fixture_stan4bart)
  td <- tidy_draws(fixture_stan4bart)

  expect_equal(vd$.sigma_sq, td$sigma^2)
  expect_equal(vd$.chain, td$.chain)
  expect_equal(vd$.iteration, td$.iteration)
  expect_equal(vd$.draw, td$.draw)
  # fixture_stan4bart is fit with chains = 2 - a real multi-chain check,
  # not just a single-chain degenerate case.
  expect_equal(sort(unique(vd$.chain)), 1:2)
})

test_that("variance_draws.stan4bartFit() respects the `value` argument", {
  skip_if(is.null(fixture_stan4bart))

  vd <- variance_draws(fixture_stan4bart, value = "myvar")
  expect_true("myvar" %in% names(vd))
  expect_equal(vd$myvar, tidy_draws(fixture_stan4bart)$sigma^2)
})

test_that("variance_draws.stan4bartFit() errors for a binary (probit) outcome model", {
  skip_if(is.null(fixture_stan4bart_bin))
  expect_error(variance_draws(fixture_stan4bart_bin), "not applicable")
})
