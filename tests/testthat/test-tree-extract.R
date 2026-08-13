library(BART)
library(dplyr)

tt <- tidytreatment:::posterior_trees_BART(bartmodel1)

test_that("posterior_trees_BART header counts match the tree table", {
  expect_equal(length(unique(tt$trees$iter)), tt$n_mcmc)

  per_iter_tree_counts <- tt$trees %>%
    dplyr::group_by(iter) %>%
    dplyr::summarise(n = length(unique(tree_id)), .groups = "drop")

  expect_true(all(per_iter_tree_counts$n == tt$n_tree))
})

test_that("leaf rows carry a leaf_value and no split info; stem rows the opposite", {
  expect_equal(tt$trees$is_leaf, is.na(tt$trees$child_left) & is.na(tt$trees$child_right))

  leaf_rows <- dplyr::filter(tt$trees, is_leaf)
  stem_rows <- dplyr::filter(tt$trees, !is_leaf)

  expect_true(all(is.na(leaf_rows$var)))
  expect_true(all(is.na(leaf_rows$cut)))
  expect_true(all(!is.na(leaf_rows$leaf_value)))

  expect_true(all(!is.na(stem_rows$var)))
  expect_true(all(!is.na(stem_rows$cut)))
  expect_true(all(is.na(stem_rows$leaf_value)))
})

test_that("parent/child node indices are consistent with binary tree numbering", {
  non_root <- dplyr::filter(tt$trees, node != 1L)
  expect_equal(non_root$parent, non_root$node %/% 2L)

  root_rows <- dplyr::filter(tt$trees, node == 1L)
  expect_true(all(is.na(root_rows$parent)))
})

test_that("child_left/child_right/parent index arithmetic on a hand-built tree", {
  nodes <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L)

  expect_equal(tidytreatment:::child_left(nodes), c(2L, 4L, 6L, NA, NA, NA, NA))
  expect_equal(tidytreatment:::child_right(nodes), c(3L, 5L, 7L, NA, NA, NA, NA))
  expect_equal(tidytreatment:::parent(nodes), c(NA, 1L, 1L, 2L, 2L, 3L, 3L))
})

test_that("label rounds leaf values / cut points to label_digits", {
  tt2 <- tidytreatment:::posterior_trees_BART(bartmodel1, label_digits = 1)

  leaf_rows <- dplyr::filter(tt2$trees, is_leaf)
  expect_equal(leaf_rows$label, as.character(round(leaf_rows$leaf_value, digits = 1)))
})
