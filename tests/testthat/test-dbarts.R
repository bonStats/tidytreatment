library(dplyr)

skip_if_not_installed("dbarts")

set.seed(1)
n <- 60
x_dbarts <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
y_dbarts_cont <- x_dbarts$x1 - x_dbarts$x2 + rnorm(n, sd = 0.5)
y_dbarts_bin <- rbinom(n, 1, plogis(x_dbarts$x1))

fixture_dbarts <- dbarts::bart2(
  y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
  n.trees = 20L, n.burn = 10L, n.samples = 15L, n.chains = 1L,
  keepTrees = TRUE, verbose = FALSE
)

fixture_dbarts_bin <- dbarts::bart2(
  y ~ x1 + x2, data = cbind(y = y_dbarts_bin, x_dbarts),
  n.trees = 20L, n.burn = 10L, n.samples = 15L, n.chains = 1L,
  keepTrees = TRUE, verbose = FALSE
)

fixture_dbarts_no_keeptrees <- dbarts::bart2(
  y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
  n.trees = 20L, n.burn = 10L, n.samples = 15L, n.chains = 1L,
  keepTrees = FALSE, verbose = FALSE
)

# treatment column baked directly into x.train (the two-step causal
# workflow's own layout) - for model.matrix.bart()/treatment_effects()/
# has_common_support() without an explicit newdata/modeldata.
z_dbarts <- as.integer(rbinom(n, 1, 0.5))
x_dbarts_z <- cbind(x_dbarts, data.frame(z = z_dbarts))
y_dbarts_cont_z <- x_dbarts$x1 - x_dbarts$x2 + z_dbarts + rnorm(n, sd = 0.5)

fixture_dbarts_z <- dbarts::bart2(
  y ~ x1 + x2 + z, data = cbind(y = y_dbarts_cont_z, x_dbarts_z),
  n.trees = 20L, n.burn = 10L, n.samples = 15L, n.chains = 1L,
  keepTrees = TRUE, verbose = FALSE
)

test_that("epred_draws.bart defaults to the response scale (identity for continuous, probability for binary)", {
  ed_cont <- epred_draws(fixture_dbarts, newdata = x_dbarts, include_newdata = FALSE, value = "fit")
  ed_cont_linear <- epred_draws(fixture_dbarts, newdata = x_dbarts, include_newdata = FALSE, value = "fit", scale = "linear")
  expect_equal(ed_cont, ed_cont_linear)

  ed_bin <- epred_draws(fixture_dbarts_bin, newdata = x_dbarts, include_newdata = FALSE, value = "fit")
  expect_true(all(ed_bin$fit >= 0 & ed_bin$fit <= 1))
  ed_bin_lin <- epred_draws(fixture_dbarts_bin, newdata = x_dbarts, include_newdata = FALSE, value = "fit", scale = "linear")
  expect_equal(pnorm(ed_bin_lin$fit), ed_bin$fit)
})

test_that("epred_draws.bart (no newdata) matches yhat.train, with pnorm() applied for a binary outcome", {
  ed <- epred_draws(fixture_dbarts, include_newdata = FALSE, value = "fit")
  # yhat.train is [draws x obs]: each row is a draw, each column an
  # observation - so .draw comes from the row number, .row (observation)
  # from the pivoted column index.
  check <- as_tibble(fixture_dbarts$yhat.train, .name_repair = function(nm) paste0("obscol", seq_along(nm))) %>%
    mutate(.draw = row_number()) %>%
    tidyr::pivot_longer(cols = starts_with("obscol"), names_to = ".row", values_to = "fit_check") %>%
    mutate(.row = as.integer(gsub("obscol", "", .row)))
  comp <- left_join(as.data.frame(ed), check, by = c(".row", ".draw"))
  expect_equal(comp$fit, comp$fit_check)

  ed_bin <- epred_draws(fixture_dbarts_bin, include_newdata = FALSE, value = "fit")
  expect_equal(sort(unique(ed_bin$.draw)), 1:nrow(fixture_dbarts_bin$yhat.train))
  expect_true(all(ed_bin$fit >= 0 & ed_bin$fit <= 1))
})

test_that("epred_draws.bart matches predict(..., type = 'ev'/'bart') on newdata", {
  check_ev <- predict(fixture_dbarts, newdata = x_dbarts, type = "ev", combineChains = FALSE)
  ed <- epred_draws(fixture_dbarts, newdata = x_dbarts, include_newdata = FALSE, value = "fit")
  pm <- ed %>% group_by(.row) %>% summarise(m = mean(fit)) %>% arrange(.row) %>% pull(m)
  expect_equal(pm, unname(rowMeans(t(check_ev))))
})

test_that("epred_draws.bart errors when include_newdata = TRUE and newdata missing", {
  expect_error(
    epred_draws(fixture_dbarts, include_newdata = TRUE),
    "newdata"
  )
})

test_that("predicted_draws.bart draws Normal(fit, sigma) for continuous and Bernoulli(fit) for binary", {
  pd <- predicted_draws(fixture_dbarts, newdata = x_dbarts, include_newdata = FALSE, value = "pred", include_fitted = TRUE, include_sigsqs = TRUE)
  expect_true(all(c("pred", ".fit", "sigsq") %in% names(pd)))
  resid <- pd$pred - pd$.fit
  expect_equal(mean(resid), 0, tolerance = 0.5)

  pd_bin <- predicted_draws(fixture_dbarts_bin, newdata = x_dbarts, include_newdata = FALSE, value = "pred")
  expect_true(all(pd_bin$pred %in% c(0L, 1L)))
})

test_that("epred_draws.bart correctly combines chains for a multi-chain model (regression test)", {
  # dbarts's default n.chains is 4 (not 1) - yhat.train/predict(combineChains = FALSE)
  # are then 3D [chains x draws x obs], not 2D [draws x obs]
  fit_multichain <- dbarts::bart2(
    y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
    n.trees = 15L, n.burn = 5L, n.samples = 8L, n.chains = 3L,
    keepTrees = TRUE, verbose = FALSE
  )
  expect_equal(length(dim(fit_multichain$yhat.train)), 3L)

  ed <- epred_draws(fit_multichain, newdata = x_dbarts, include_newdata = FALSE, value = "fit")
  expect_equal(nrow(ed), n * 3L * 8L)
  expect_equal(sort(unique(ed$.draw)), 1:24)

  check <- predict(fit_multichain, newdata = x_dbarts, type = "bart", combineChains = FALSE)
  d <- dim(check)
  check_flat <- matrix(aperm(check, c(2, 1, 3)), nrow = d[1] * d[2], ncol = d[3])
  # check_flat is [combined draws x obs], same orientation as yhat.train.
  check_df <- as_tibble(check_flat, .name_repair = function(nm) paste0("obscol", seq_along(nm))) %>%
    mutate(.draw = row_number()) %>%
    tidyr::pivot_longer(cols = starts_with("obscol"), names_to = ".row", values_to = "fit_check") %>%
    mutate(.row = as.integer(gsub("obscol", "", .row)))

  comp <- left_join(as.data.frame(ed), check_df, by = c(".row", ".draw"))
  expect_equal(comp$fit, comp$fit_check)
})

test_that("epred_draws.bart include_sigsqs stays aligned by .draw for a multi-chain model", {
  fit_multichain <- dbarts::bart2(
    y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
    n.trees = 15L, n.burn = 5L, n.samples = 8L, n.chains = 3L,
    keepTrees = TRUE, verbose = FALSE
  )
  expect_true(is.matrix(fit_multichain$sigma))

  ed <- epred_draws(fit_multichain, newdata = x_dbarts, include_newdata = FALSE, value = "fit", include_sigsqs = TRUE)
  sigsq_by_draw <- ed %>% ungroup() %>% distinct(.draw, sigsq) %>% arrange(.draw)
  expect_equal(sigsq_by_draw$sigsq, as.vector(t(fit_multichain$sigma))^2)
})

test_that("epred_draws.bart() recovers real .chain/.iteration for a multi-chain model (regression test)", {
  fit_multichain <- dbarts::bart2(
    y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
    n.trees = 15L, n.burn = 5L, n.samples = 8L, n.chains = 3L,
    keepTrees = TRUE, verbose = FALSE
  )

  expected <- dplyr::tibble(
    .draw = 1:24,
    .chain = rep(1:3, each = 8L),
    .iteration = rep(1:8, times = 3L)
  )

  # no newdata (yhat.train path)
  ed_train <- epred_draws(fit_multichain, include_newdata = FALSE, value = "fit")
  chain_train <- ed_train %>% ungroup() %>% distinct(.draw, .chain, .iteration) %>% arrange(.draw)
  expect_equal(chain_train, expected)

  # newdata (predict() path)
  ed_newdata <- epred_draws(fit_multichain, newdata = x_dbarts, include_newdata = FALSE, value = "fit")
  chain_newdata <- ed_newdata %>% ungroup() %>% distinct(.draw, .chain, .iteration) %>% arrange(.draw)
  expect_equal(chain_newdata, expected)

  # a specific value, checked directly against yhat.train's own array indexing
  row1draw9 <- ed_train %>% filter(.row == 1, .draw == 9)
  expect_equal(row1draw9$.chain, 2L)
  expect_equal(row1draw9$.iteration, 1L)
  expect_equal(row1draw9$fit, fit_multichain$yhat.train[2, 1, 1])
})

test_that("epred_draws.bart() sets .chain = 1L (not NA) for a single-chain model", {
  ed <- epred_draws(fixture_dbarts, include_newdata = FALSE, value = "fit")
  expect_true(all(ed$.chain == 1L))
  expect_equal(sort(unique(ed$.iteration)), 1:15)
})

test_that("residual_draws.bart()/variance_draws.bart() also recover real .chain/.iteration", {
  fit_multichain <- dbarts::bart2(
    y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
    n.trees = 15L, n.burn = 5L, n.samples = 8L, n.chains = 3L,
    keepTrees = TRUE, verbose = FALSE
  )

  rd <- residual_draws(fit_multichain, include_newdata = FALSE)
  expect_equal(sort(unique(rd$.chain)), 1:3)

  vd <- variance_draws(fit_multichain)
  expect_equal(vd$.chain, rep(1:3, each = 8L))
  expect_equal(vd$.iteration, rep(1:8, times = 3L))
})

test_that("epred_draws()/predicted_draws()/has_common_support()/tidy_draws() work on a bartc() fit's fit.rsp made without a parametric argument", {
  skip_if_not_installed("bartCause")

  z <- as.integer(rbinom(n, 1, 0.5))
  y <- x_dbarts$x1 - x_dbarts$x2 + z + rnorm(n, sd = 0.5)
  dat <- cbind(x_dbarts, data.frame(y = y, z = z))

  fit <- bartCause::bartc(
    response = y, treatment = z, confounders = x1 + x2, data = dat,
    method.rsp = "bart", method.trt = "bart",
    args.rsp = list(n.trees = 10L, n.burn = 5L, n.samples = 10L, keepTrees = TRUE),
    args.trt = list(n.trees = 10L, n.burn = 5L, n.samples = 10L, keepTrees = TRUE),
    seed = NA_integer_, verbose = FALSE
  )
  expect_false(inherits(fit$fit.rsp, "stan4bartFit"))
  expect_true(inherits(fit$fit.rsp, "bart"))

  ed <- epred_draws(fit, value = "fitted")
  expect_equal(nrow(ed), n * fit$n.chains * 10L)
  expect_equal(sort(unique(ed$.chain)), seq_len(fit$n.chains))
  expect_equal(as.integer(table(ed$.chain)) / n, rep(10L, fit$n.chains))

  modeldata <- cbind(x_dbarts, data.frame(z = z, ps = fit$p.score))
  cs <- has_common_support(fit$fit.rsp, treatment = "z", method = "sd", modeldata = modeldata)
  expect_equal(nrow(cs), n)
  expect_type(cs$common_support, "logical")

  td <- tidybayes::tidy_draws(fit)
  expect_equal(nrow(td), fit$n.chains * 10L)
  expect_true("sigma" %in% names(td))
  expect_equal(sort(unique(td$.chain)), seq_len(fit$n.chains))
})

test_that("dbarts_is_binary()'s two signals (sigma-absence, binaryOffset-slot-existence) agree for real bart2() fits", {
  # bart2() carries the `binaryOffset` slot for a binary fit but leaves its value NULL
  expect_false(tidytreatment:::dbarts_is_binary(fixture_dbarts))
  expect_true(tidytreatment:::dbarts_is_binary(fixture_dbarts_bin))
})

test_that("dbarts_is_binary() errors if sigma-absence and binaryOffset-slot-existence ever disagree", {
  fake_model <- list(sigma = numeric(0))
  expect_error(tidytreatment:::dbarts_is_binary(fake_model), "disagree")
})

test_that("model.matrix.bart() exactly reconstructs the training data, restoring 0/1 columns to integer", {
  mm <- model.matrix(fixture_dbarts_z)
  expect_equal(mm[, c("x1", "x2")], x_dbarts, ignore_attr = TRUE)
  expect_true(is.integer(mm$z))
  expect_equal(mm$z, z_dbarts)
})

test_that("model.matrix.bart() errors informatively when the model was fit with keepTrees = FALSE", {
  expect_error(model.matrix(fixture_dbarts_no_keeptrees), "keeptrees")
})

test_that("tidy_draws.bart() returns sigma for a continuous outcome model, no k (fixed, not sampled)", {
  td <- tidy_draws(fixture_dbarts)
  expect_equal(nrow(td), nrow(fixture_dbarts$yhat.train))
  expect_true(all(td$.chain == 1L))
  expect_equal(td$sigma, fixture_dbarts$sigma)
  expect_false("k" %in% names(td))
})

test_that("tidy_draws.bart() returns k (not sigma) for a binary outcome model with k's default hyperprior", {
  td <- tidy_draws(fixture_dbarts_bin)
  expect_false("sigma" %in% names(td))

  if (!is.null(fixture_dbarts_bin$k)) {
    expect_equal(td$k, fixture_dbarts_bin$k)
  } else {
    expect_false("k" %in% names(td))
  }
})

test_that("tidy_draws.bart() recovers real .chain/.iteration and per-chain k for a multi-chain binary model", {
  fit_multichain <- dbarts::bart2(
    y ~ x1 + x2, data = cbind(y = y_dbarts_bin, x_dbarts),
    n.trees = 15L, n.burn = 5L, n.samples = 8L, n.chains = 3L,
    keepTrees = TRUE, verbose = FALSE
  )
  expect_true(is.matrix(fit_multichain$k))

  td <- tidy_draws(fit_multichain)
  expect_equal(nrow(td), 24L)
  expect_equal(sort(unique(td$.chain)), 1:3)
  expect_equal(td$k, as.vector(t(fit_multichain$k)))
})

test_that("residual_draws.bart() defaults `response` to the model's own stored response", {
  rd_default <- residual_draws(fixture_dbarts, include_newdata = FALSE)
  rd_explicit <- residual_draws(fixture_dbarts, response = y_dbarts_cont, include_newdata = FALSE)
  expect_equal(rd_default, rd_explicit)

  fitted <- epred_draws(fixture_dbarts, include_newdata = FALSE, value = "fit")
  expect_equal(rd_default$.residual, y_dbarts_cont[rd_default$.row] - fitted$fit)
})

test_that("residual_draws.bart() works for a binary (probit) outcome model, on the probability scale", {
  rd_bin <- residual_draws(fixture_dbarts_bin, include_newdata = FALSE)
  fitted_bin <- epred_draws(fixture_dbarts_bin, include_newdata = FALSE, value = "fit")
  expect_equal(rd_bin$.residual, y_dbarts_bin[rd_bin$.row] - fitted_bin$fit)
})

test_that("residual_draws.bart() errors when `response` is missing and can't be recovered", {
  expect_error(residual_draws(fixture_dbarts_no_keeptrees), "response")
})

test_that("variance_draws.bart() exactly matches the model's own sigma draws, squared", {
  vd <- variance_draws(fixture_dbarts)
  expect_equal(vd$.sigma_sq, fixture_dbarts$sigma^2)
})

test_that("variance_draws.bart() errors for a binary (probit) outcome model", {
  expect_error(variance_draws(fixture_dbarts_bin), "binary")
})

test_that("covariate_importance.bart() exactly matches the model's own varcount, averaged over draws", {
  ci <- covariate_importance(fixture_dbarts)
  expect_equal(unname(ci$avg_inclusion), unname(colMeans(fixture_dbarts$varcount)))
  expect_equal(ci$variable, colnames(fixture_dbarts$varcount))
})

test_that("covariate_importance.bart() correctly combines chains for a multi-chain model", {
  fit_multichain <- dbarts::bart2(
    y ~ x1 + x2, data = cbind(y = y_dbarts_cont, x_dbarts),
    n.trees = 15L, n.burn = 5L, n.samples = 8L, n.chains = 3L,
    keepTrees = TRUE, verbose = FALSE
  )
  expect_equal(length(dim(fit_multichain$varcount)), 3L)

  ci <- covariate_importance(fit_multichain)
  var_names <- dimnames(fit_multichain$varcount)[[3]]
  expect_equal(unname(ci$avg_inclusion), unname(apply(fit_multichain$varcount, 3, mean))[match(ci$variable, var_names)])
})

test_that("treatment_effects()/has_common_support() work on a dbarts::bart2() model without an explicit newdata/modeldata", {
  expect_true(has_tidytreatment_methods(fixture_dbarts_z))

  te <- treatment_effects(fixture_dbarts_z, treatment = "z")
  expect_equal(nrow(te), n * 15L)

  cs <- has_common_support(fixture_dbarts_z, treatment = "z", method = "sd")
  expect_equal(nrow(cs), n)
  expect_type(cs$common_support, "logical")
})
