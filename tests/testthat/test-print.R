test_that("print.wbart reports sample count and component names", {
  expect_output(print(bartmodel1), "BART::wbart")
  expect_output(print(bartmodel1), "components:")
  expect_output(print(bartmodel1), as.character(length(bartmodel1$yhat.train.mean)))
})

test_that("print methods for other BART-package classes report the right class label", {
  fake <- function(cls) structure(list(yhat.train.mean = 1:5, foo = "bar"), class = cls)

  expect_output(print(fake("pbart")), "BART::pbart")
  expect_output(print(fake("lbart")), "BART:lbart")
  expect_output(print(fake("mbart")), "BART::mbart")
  expect_output(print(fake("mbart2")), "BART::mbart2")
})

test_that("print methods list component names with a $ prefix", {
  fake <- structure(list(yhat.train.mean = 1:3, alpha = 1, beta = 2), class = "wbart")

  out <- capture.output(print(fake))
  expect_true(any(grepl("\\$alpha", out)))
  expect_true(any(grepl("\\$beta", out)))
})
