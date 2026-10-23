library(BART)
library(dplyr)

test_that("predicted_draws.wbart(include_fitted = TRUE) attaches .fit (regression test: include_fitted was previously never forwarded)", {
  pred_default <- predicted_draws(bartmodel1, newdata = suhillsim1$data, include_newdata = FALSE, value = "pred")
  expect_false(".fit" %in% names(pred_default))

  pred_fitted <- predicted_draws(bartmodel1, newdata = suhillsim1$data, include_newdata = FALSE, value = "pred", include_fitted = TRUE)
  expect_true(".fit" %in% names(pred_fitted))
})

test_that("tidy_draws.wbart() returns sigma, tail-aligned and chain-labelled the same way epred_draws.wbart() is", {
  td <- tidy_draws(bartmodel1)
  expect_equal(nrow(td), nrow(bartmodel1$yhat.train))
  expect_true(all(td$.chain == 1L))
  expect_equal(td$sigma, utils::tail(bartmodel1$sigma, nrow(bartmodel1$yhat.train)))
})

test_that("tidy_draws.pbart()/.lbart() return .chain/.iteration/.draw with no parameter columns (no sigma-equivalent to expose)", {
  set.seed(1)
  n <- 20
  x <- data.frame(x1 = rnorm(n))
  y <- rbinom(n, 1, plogis(x$x1))
  fit_p <- BART::pbart(x.train = x, y.train = y, ntree = 5L, ndpost = 8L, nskip = 3L, printevery = 100000L)
  fit_l <- BART::lbart(x.train = x, y.train = y, ntree = 5L, ndpost = 8L, nskip = 3L, printevery = 100000L)

  td_p <- tidy_draws(fit_p)
  td_l <- tidy_draws(fit_l)

  expect_equal(names(td_p), c(".chain", ".iteration", ".draw"))
  expect_equal(names(td_l), c(".chain", ".iteration", ".draw"))
  expect_true(all(is.na(td_p$.chain)))
  expect_equal(nrow(td_p), nrow(fit_p$yhat.train))
})

test_that("tidy_draws.wbart() recovers real .chain/.iteration and tail-aligned per-chain sigma for a multi-chain model", {
  skip_on_os("windows")
  skip_if_not_installed("BART")

  set.seed(1)
  n <- 30
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- x$x1 - x$x2 + rnorm(n)
  fit <- BART::mc.wbart(x.train = x, y.train = y, ntree = 5L, ndpost = 12L, nskip = 5L, mc.cores = 3L, printevery = 100000L)

  n_chains <- ncol(fit$sigma)
  n_per_chain <- nrow(fit$yhat.train) / n_chains
  expected_sigma <- as.vector(utils::tail(fit$sigma, n_per_chain))

  td <- tidy_draws(fit)
  expect_equal(sort(unique(td$.chain)), seq_len(n_chains))
  expect_equal(td$sigma, expected_sigma)
})

test_that("epred_draws.wbart() sets .chain = 1L (not NA) for a single-chain model", {
  # bartmodel1$sigma is a plain vector, not a matrix - one chain
  ed <- epred_draws(bartmodel1, newdata = suhillsim1$data, include_newdata = FALSE, value = "fitted")
  expect_true(all(ed$.chain == 1L))
  expect_equal(sort(unique(ed$.iteration)), seq_len(nrow(bartmodel1$yhat.train)))
})

test_that("epred_draws.pbart()/.lbart() leave .chain/.iteration as NA (not recoverable, unlike wbart)", {
  set.seed(1)
  n <- 20
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- rbinom(n, 1, plogis(x$x1))
  fit <- BART::pbart(x.train = x, y.train = y, ntree = 5L, ndpost = 8L, nskip = 3L, printevery = 100000L)

  ed <- epred_draws(fit, newdata = x, include_newdata = FALSE, value = "fitted")
  expect_true(all(is.na(ed$.chain)))
  expect_true(all(is.na(ed$.iteration)))
})

test_that("epred_draws.wbart() recovers real .chain/.iteration for an mc.wbart() multi-chain model (regression test)", {
  skip_on_os("windows") # mc.wbart() uses parallel::mcparallel(), fork-based, unavailable on Windows
  skip_if_not_installed("BART")

  set.seed(1)
  n <- 30
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- x$x1 - x$x2 + rnorm(n)

  fit <- BART::mc.wbart(x.train = x, y.train = y, ntree = 5L, ndpost = 12L, nskip = 5L, mc.cores = 3L, printevery = 100000L)
  expect_true(is.matrix(fit$sigma))
  n_chains <- ncol(fit$sigma)
  n_per_chain <- nrow(fit$yhat.train) / n_chains

  expected <- dplyr::tibble(
    .draw = seq_len(nrow(fit$yhat.train)),
    .chain = rep(seq_len(n_chains), each = n_per_chain),
    .iteration = rep(seq_len(n_per_chain), times = n_chains)
  )

  # no newdata (yhat.train path)
  ed_train <- epred_draws(fit, include_newdata = FALSE, value = "fit")
  chain_train <- ed_train %>% ungroup() %>% distinct(.draw, .chain, .iteration) %>% arrange(.draw)
  expect_equal(chain_train, expected)

  # newdata (predict() path)
  ed_newdata <- epred_draws(fit, newdata = x, include_newdata = FALSE, value = "fit")
  chain_newdata <- ed_newdata %>% ungroup() %>% distinct(.draw, .chain, .iteration) %>% arrange(.draw)
  expect_equal(chain_newdata, expected)

  # residual_draws.wbart() inherits .chain/.iteration via delegation to epred_draws()
  rd <- residual_draws(fit, response = y, include_newdata = FALSE)
  expect_equal(sort(unique(rd$.chain)), seq_len(n_chains))
})

test_that("epred_draws.wbart(include_sigsqs = TRUE) attaches the post-burn-in sigma draws, not the burn-in ones (regression test)", {
  set.seed(1)
  n <- 20
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- x$x1 - x$x2 + rnorm(n)
  fit <- BART::wbart(x.train = x, y.train = y, ntree = 5L, ndpost = 12L, nskip = 5L, printevery = 100000L)
  expect_equal(length(fit$sigma), 12L + 5L)

  ed <- epred_draws(fit, newdata = x, include_newdata = FALSE, value = "fit", include_sigsqs = TRUE)
  sigsq_by_draw <- ed %>% ungroup() %>% distinct(.draw, sigsq) %>% arrange(.draw)

  expect_equal(sigsq_by_draw$sigsq, utils::tail(fit$sigma, 12L)^2)
  expect_false(isTRUE(all.equal(sigsq_by_draw$sigsq, utils::head(fit$sigma, 12L)^2)))
})

test_that("predicted_draws.wbart() draws noise using the corrected (post-burn-in) sigma", {
  set.seed(1)
  n <- 20
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- x$x1 - x$x2 + rnorm(n)
  fit <- BART::wbart(x.train = x, y.train = y, ntree = 5L, ndpost = 12L, nskip = 5L, printevery = 100000L)

  pd <- predicted_draws(fit, newdata = x, include_newdata = FALSE, value = "pred", include_sigsqs = TRUE)
  expect_equal(sort(unique(pd$sigsq)), sort(utils::tail(fit$sigma, 12L)^2))
})

test_that("epred_draws.wbart(include_sigsqs = TRUE) tail-aligns sigma per chain for a multi-chain model", {
  skip_on_os("windows")
  skip_if_not_installed("BART")

  set.seed(1)
  n <- 20
  x <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  y <- x$x1 - x$x2 + rnorm(n)
  fit <- BART::mc.wbart(x.train = x, y.train = y, ntree = 5L, ndpost = 12L, nskip = 5L, mc.cores = 3L, printevery = 100000L)

  n_chains <- ncol(fit$sigma)
  n_per_chain <- nrow(fit$yhat.train) / n_chains
  expected_sigma <- as.vector(utils::tail(fit$sigma, n_per_chain))

  ed <- epred_draws(fit, newdata = x, include_newdata = FALSE, value = "fit", include_sigsqs = TRUE)
  sigsq_by_draw <- ed %>% ungroup() %>% distinct(.draw, sigsq) %>% arrange(.draw)
  expect_equal(sigsq_by_draw$sigsq, expected_sigma^2)
})
