test_that("matrix_to_mcmclist splits chains along the first dimension", {
  arr <- matrix(1:6, nrow = 2, ncol = 3) # 2 chains x 3 samples

  ml <- tidytreatment:::matrix_to_mcmclist(arr, sample = 2, chain = 1)

  expect_s3_class(ml, "mcmc.list")
  expect_equal(length(ml), 2)
  expect_equal(as.numeric(ml[[1]]), arr[1, ])
  expect_equal(as.numeric(ml[[2]]), arr[2, ])
})

test_that("matrix_to_mcmclist splits chains along the second dimension", {
  arr <- matrix(1:6, nrow = 3, ncol = 2) # 3 samples x 2 chains

  ml <- tidytreatment:::matrix_to_mcmclist(arr, sample = 1, chain = 2)

  expect_equal(length(ml), 2)
  expect_equal(as.numeric(ml[[1]]), arr[, 1])
  expect_equal(as.numeric(ml[[2]]), arr[, 2])
})

test_that("matrix_to_mcmclist validates that sample/chain span {1, 2}", {
  arr <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(tidytreatment:::matrix_to_mcmclist(arr, sample = 1, chain = 1))
})

test_that("array_to_mcmclist reshapes to sample x variable per chain (chain = last dim)", {
  set.seed(1)
  arr <- array(rnorm(2 * 5 * 3), dim = c(2, 5, 3)) # variable x sample x chain

  ml <- tidytreatment:::array_to_mcmclist(arr, sample = 2, variable = 1, chain = 3)

  expect_s3_class(ml, "mcmc.list")
  expect_equal(length(ml), 3)
  for (ch in 1:3) {
    expect_equal(dim(ml[[ch]]), c(5, 2))
    expect_equal(as.numeric(ml[[ch]]), as.numeric(t(arr[, , ch])))
  }
})

test_that("array_to_mcmclist reshapes to sample x variable per chain (chain = first dim)", {
  set.seed(2)
  arr <- array(rnorm(3 * 5 * 2), dim = c(3, 5, 2)) # chain x sample x variable

  ml <- tidytreatment:::array_to_mcmclist(arr, sample = 2, variable = 3, chain = 1)

  expect_equal(length(ml), 3)
  for (ch in 1:3) {
    expect_equal(dim(ml[[ch]]), c(5, 2))
    expect_equal(as.numeric(ml[[ch]]), as.numeric(arr[ch, , ]))
  }
})

test_that("array_to_mcmclist validates that sample/variable/chain span {1, 2, 3}", {
  arr <- array(1:8, dim = c(2, 2, 2))
  expect_error(tidytreatment:::array_to_mcmclist(arr, sample = 1, variable = 1, chain = 2))
})
