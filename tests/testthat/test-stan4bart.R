library(dplyr)

skip_if_not_installed("stan4bart")
skip_if(is.null(fixture_stan4bart))
skip_if(is.null(fixture_stan4bart_bin))

test_that("epred_draws.stan4bartFit returns one row per observation per post-warmup draw", {
  ed <- epred_draws(fixture_stan4bart, value = "fitted")

  # fixture fit with iter = 60, chains = 2 => warmup = 30, 30 post-warmup draws/chain;
  # `.draw` is a globally unique index across chains under tidybayes conventions
  n_draws <- length(unique(ed$.draw))
  expect_equal(n_draws, 60)
  expect_equal(nrow(ed), nrow(fixture_stan4bart_data) * n_draws)
  expect_true(all(c(".row", ".chain", ".iteration", ".draw", "fitted") %in% names(ed)))
})

test_that("epred_draws.stan4bartFit works with newdata when keepTrees = TRUE", {
  newdat <- fixture_stan4bart_data[1:5, ]
  ed <- suppressWarnings(epred_draws(fixture_stan4bart, newdata = newdat, value = "fitted"))

  expect_equal(sort(unique(ed$.row)), 1:5)
})

test_that("linpred_draws.stan4bartFit (gaussian) delegates to epred_draws", {
  ep <- epred_draws(fixture_stan4bart, value = "ep")
  lp <- linpred_draws(fixture_stan4bart, value = "lp")

  comp <- dplyr::left_join(
    as.data.frame(ep), as.data.frame(lp),
    by = c(".row", ".chain", ".iteration", ".draw")
  )
  expect_equal(comp$lp, comp$ep)
})

test_that("tidy_draws.stan4bartFit returns per-draw parameters including sigma", {
  td <- tidy_draws(fixture_stan4bart)

  expect_true(all(c(".chain", ".iteration", ".draw", "sigma") %in% names(td)))
})

test_that("epred_draws.stan4bartFit (bernoulli) is on the probability scale", {
  ep <- epred_draws(fixture_stan4bart_bin, value = "ep")
  expect_true(all(ep$ep >= 0 & ep$ep <= 1))
})

test_that("predicted_draws.stan4bartFit (bernoulli) draws 0/1 outcomes", {
  pd <- predicted_draws(fixture_stan4bart_bin, value = "pred")
  expect_true(all(pd$pred %in% c(0, 1)))
})

test_that("linpred_draws.stan4bartFit (bernoulli) sums fixed + random + bart linear predictor components", {
  sample_array <- Reduce("+", lapply(
    c("indiv.fixef", "indiv.ranef", "indiv.bart"),
    function(type) dbarts::extract(object = fixture_stan4bart_bin, type = type, combine_chains = FALSE)
  ))

  check <- tidytreatment:::array_to_mcmclist(sample_array, 2, 1, 3) %>%
    tidybayes::tidy_draws() %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = "lp_check") %>%
    dplyr::mutate(.row = as.integer(gsub("var", "", .row)))

  lp <- linpred_draws(fixture_stan4bart_bin, value = "lp")

  comp <- dplyr::left_join(
    as.data.frame(lp), as.data.frame(check),
    by = c(".chain", ".iteration", ".draw", ".row")
  )
  expect_equal(comp$lp, comp$lp_check)
})
