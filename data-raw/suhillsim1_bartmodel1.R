## Code to prepare `suhillsim1` and `cached_bart_model1`
library(tidytreatment)
library(BART)
library(dplyr)
tidytreatment:::with_seed(101, {

  sim <- simulate_su_hill_data(n = 200, treatment_linear = F,  omega = 0, add_categorical = T,
                               coef_categorical_treatment = c(0,0,1),
                               coef_categorical_nontreatment = c(-1,0,-1)
  )

  # regress y ~ covariates
  var_select_bart <- wbart(x.train = select(sim$data,-y,-z),
                           y.train = pull(sim$data, y),
                           sparse = T,
                           nskip = 2000,
                           ndpost = 5000)

  # select most important vars from y ~ covariates model
  # very simple selection mechanism. Should use cross-validation in practice
  var_select <- var_select_bart$varprob.mean %>% {which(. > 1/length(.))} %>% names()
  # change categoricals to just one variable
  var_select <- gsub("c1[1-3]$","c1",var_select)

  # regress z ~ most important covariates to get propensity score
  # BART::pbart is for probit regression
  prop_bart <- pbart(
    x.train = select(sim$data, all_of(var_select)),
    y.train = pull(sim$data, z),
    nskip = 2000,
    ndpost = 5000
  )

  sim$data$prop_score <-  prop_bart$prob.train.mean

  bmodel <- wbart(
    x.train = select(sim$data,-y),
    y.train = pull(sim$data, y),
    nskip = 10000L,
    ndpost = 200L, # keep small to manage size on CRAN
    keepevery = 100L,
    printevery= 3000L
  )


})

suhillsim1 <- sim
bartmodel1 <- bart_model

usethis::use_data(suhillsim1, overwrite = T)
usethis::use_data(bartmodel1, overwrite = T)
