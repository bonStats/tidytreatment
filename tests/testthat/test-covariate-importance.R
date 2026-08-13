library(BART)
library(dplyr)

test_that("covariate_importance.wbart returns varcount.mean as a tibble", {
  res <- covariate_importance(bartmodel1)

  expect_s3_class(res, "tbl_df")
  expect_setequal(res$variable, names(bartmodel1$varcount.mean))

  expected <- dplyr::tibble(variable = names(bartmodel1$varcount.mean), avg_inclusion = unname(bartmodel1$varcount.mean))
  actual <- dplyr::arrange(res, variable)
  actual$avg_inclusion <- unname(actual$avg_inclusion)
  expect_equal(actual, dplyr::arrange(expected, variable))
})

test_that("covariate_with_treatment_importance.wbart excludes the treatment variable", {
  res <- covariate_with_treatment_importance(bartmodel1, treatment = "z")

  expect_false("z" %in% res$variable)
  expect_setequal(res$variable, setdiff(names(bartmodel1$varprob.mean), "z"))
  expect_true(all(res$avg_inclusion >= 0))
})

test_that("covariate_with_treatment_importance.wbart pads variables never co-occurring with treatment as zero", {
  res <- covariate_with_treatment_importance(bartmodel1, treatment = "z")

  # every predictor (besides treatment) should appear exactly once
  expect_equal(anyDuplicated(res$variable), 0L)
  expect_true(all(!is.na(res$avg_inclusion)))
})

ttree_ungrouped <- local({
  tt <- tidytreatment:::posterior_trees_BART(bartmodel1)
  dplyr::ungroup(tt$trees) # avoid dplyr::count() aggregating within iter/tree_id groups
})

# sanity check: fixture actually exercises the "treatment splits on >1 node
# in the same tree" case, so the two counting modes below can genuinely differ
dup_trees <- ttree_ungrouped %>%
  dplyr::filter(var == "z") %>%
  dplyr::count(iter, tree_id) %>%
  dplyr::filter(n > 1)
test_that("fixture exercises multi-split-per-tree case (sanity check)", {
  expect_gt(nrow(dup_trees), 0)
})

test_that("covariate_with_treatment_importance.wbart (count_once_per_tree = FALSE, default) multiplies by treatment's occurrence count in each tree", {
  # ground truth: exactly what the many-to-many join computes (one match per
  # treatment occurrence in a tree), i.e. no deduplication of qualifying trees
  # one row per treatment split (dupes kept), reduced to just the join keys
  # so overlapping non-key columns (e.g. `var`) don't get join-suffixed
  qualifying_occurrences <- dplyr::select(dplyr::filter(ttree_ungrouped, var == "z"), iter, tree_id)

  expected_counts <- dplyr::inner_join(qualifying_occurrences, ttree_ungrouped, by = c("iter", "tree_id"), relationship = "many-to-many") %>%
    dplyr::filter(var != "z") %>%
    dplyr::count(var, name = "avg_inclusion")

  res_default <- covariate_with_treatment_importance(bartmodel1, treatment = "z") %>%
    dplyr::filter(variable %in% expected_counts$var) %>%
    dplyr::arrange(variable)
  res_explicit_false <- covariate_with_treatment_importance(bartmodel1, treatment = "z", count_once_per_tree = FALSE) %>%
    dplyr::filter(variable %in% expected_counts$var) %>%
    dplyr::arrange(variable)

  expected <- expected_counts$avg_inclusion[match(res_default$variable, expected_counts$var)]
  expect_equal(res_default$avg_inclusion, expected)
  expect_equal(res_explicit_false$avg_inclusion, expected)
})

test_that("covariate_with_treatment_importance.wbart (count_once_per_tree = TRUE) counts each qualifying tree once", {
  # ground truth: each tree with treatment counted once, regardless of how
  # many nodes within it split on treatment
  qualifying_trees <- ttree_ungrouped %>%
    dplyr::filter(var == "z") %>%
    dplyr::distinct(iter, tree_id)

  expected_counts <- dplyr::inner_join(qualifying_trees, ttree_ungrouped, by = c("iter", "tree_id")) %>%
    dplyr::filter(var != "z") %>%
    dplyr::count(var, name = "avg_inclusion")

  res <- covariate_with_treatment_importance(bartmodel1, treatment = "z", count_once_per_tree = TRUE) %>%
    dplyr::filter(variable %in% expected_counts$var) %>%
    dplyr::arrange(variable)

  expect_equal(res$avg_inclusion, expected_counts$avg_inclusion[match(res$variable, expected_counts$var)])
})

test_that("count_once_per_tree = TRUE gives counts no larger than the default (FALSE)", {
  res_false <- covariate_with_treatment_importance(bartmodel1, treatment = "z") %>% dplyr::arrange(variable)
  res_true <- covariate_with_treatment_importance(bartmodel1, treatment = "z", count_once_per_tree = TRUE) %>% dplyr::arrange(variable)

  expect_equal(res_false$variable, res_true$variable)
  expect_true(all(res_true$avg_inclusion <= res_false$avg_inclusion))
  # and strictly less for at least one variable, since the fixture has
  # multi-split trees that inflate the FALSE (default) counts
  expect_true(any(res_true$avg_inclusion < res_false$avg_inclusion))
})

skip_if_not_installed("BART")

test_that("covariate_importance.pbart returns varcount.mean as a tibble", {
  skip_if(is.null(fixture_pbart))

  res <- covariate_importance(fixture_pbart)

  expect_setequal(res$variable, names(fixture_pbart$varcount.mean))

  expected <- dplyr::tibble(variable = names(fixture_pbart$varcount.mean), avg_inclusion = unname(fixture_pbart$varcount.mean))
  actual <- dplyr::arrange(res, variable)
  actual$avg_inclusion <- unname(actual$avg_inclusion)
  expect_equal(actual, dplyr::arrange(expected, variable))
})

test_that("covariate_importance.lbart returns varcount.mean as a tibble", {
  skip_if(is.null(fixture_lbart))

  res <- covariate_importance(fixture_lbart)

  expect_setequal(res$variable, names(fixture_lbart$varcount.mean))
})

test_that("covariate_with_treatment_importance.pbart excludes the treatment variable", {
  skip_if(is.null(fixture_pbart))

  res <- covariate_with_treatment_importance(fixture_pbart, treatment = "x1")

  expect_false("x1" %in% res$variable)
})

test_that("covariate_importance.stan4bartFit matches dbarts varcount extraction", {
  skip_if_not_installed("stan4bart")
  skip_if(is.null(fixture_stan4bart))

  vv <- dbarts::extract(fixture_stan4bart, type = "varcount", combine_chains = FALSE, include_warmup = FALSE)
  check <- dplyr::tibble(variable = dimnames(vv)$predictor, avg_inclusion = rowMeans(vv))

  res <- covariate_importance(fixture_stan4bart)

  expect_setequal(res$variable, check$variable)
  expect_equal(
    res$avg_inclusion[match(check$variable, res$variable)],
    check$avg_inclusion
  )
})

test_that("covariate_importance.bartcFit dispatches on fitstage", {
  skip_if_not_installed("bartCause")
  skip_if_not_installed("lme4")
  skip_if(is.null(fixture_bartc))

  res_rsp <- covariate_importance(fixture_bartc)
  res_rsp_explicit <- covariate_importance(fixture_bartc, fitstage = "response")
  res_trt <- covariate_importance(fixture_bartc, fitstage = "assignment")

  expect_equal(res_rsp, res_rsp_explicit)
  expect_equal(res_rsp, covariate_importance(fixture_bartc$fit.rsp))
  expect_equal(res_trt, covariate_importance(fixture_bartc$fit.trt))
})
