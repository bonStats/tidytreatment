## Code to prepare `suhillsim1` and `cached_bart_model1`
library(tidytreatment)
library(BART)
library(dplyr)
withr::with_seed(101, {
  sim <- simulate_su_hill_data(
    n = 100, treatment_linear = F, omega = 0, add_categorical = T,
    coef_categorical_treatment = c(0, 0, 1),
    coef_categorical_nontreatment = c(-1, 0, -1)
  )

  # regress y ~ covariates
  var_select_bart <- wbart(
    x.train = select(sim$data, -y, -z),
    y.train = pull(sim$data, y),
    sparse = T,
    nskip = 2000,
    ndpost = 5000,
    printevery = 1000L
  )

  # select most important vars from y ~ covariates model
  # very simple selection mechanism. Should use cross-validation in practice
  var_select <- var_select_bart$varprob.mean %>%
    {
      which(. > 1 / length(.))
    } %>%
    names()
  # change categoricals to just one variable
  var_select <- gsub("c1[1-3]$", "c1", var_select)

  # regress z ~ most important covariates to get propensity score
  # BART::pbart is for probit regression
  prop_bart <- pbart(
    x.train = select(sim$data, all_of(var_select)),
    y.train = pull(sim$data, z),
    nskip = 2000,
    ndpost = 5000,
    printevery = 1000L
  )

  sim$data$prop_score <- prop_bart$prob.train.mean

  x.train <- select(sim$data, -y)
  y.train <- pull(sim$data, y)

  bmodel <- wbart(
    x.train = x.train,
    y.train = y.train,
    nskip = 10000L,
    ndpost = 200L, # keep small to manage size on CRAN
    keepevery = 100L,
    printevery = 3000L
  )

  datamatrix1 <- bartModelMatrix(X = x.train)
})

suhillsim1 <- sim
bartmodel1 <- bmodel
bartmodel1_modelmatrix <- datamatrix1

usethis::use_data(suhillsim1, overwrite = T)
usethis::use_data(bartmodel1, overwrite = T)
usethis::use_data(bartmodel1_modelmatrix, overwrite = T)
