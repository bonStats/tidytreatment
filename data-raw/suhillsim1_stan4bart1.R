## Code to prepare `suhillsim_ranef1` and `stan4bartmodel1`
library(tidytreatment)
library(dbarts)
library(stan4bart)
library(dplyr)
withr::with_seed(101, {
  sim <- simulate_su_hill_data(
    n = 100, treatment_linear = F, omega = 0, add_categorical = T,
    coef_categorical_treatment = c(0, 0, 1),
    coef_categorical_nontreatment = c(-1, 0, -1)
  )

  # add subject effects
  nsubj <- 10
  sim$data <-  sim$data %>% mutate(group_id = factor(sample(1:nsubj, nrow(sim$data), replace = T)))
  subject_effect <- rnorm(nsubj, sd = 2)
  sim$data$y <- sim$data$y + subject_effect[sim$data$group_id]

  # regress y ~ covariates
  var_select_bart <- stan4bart(y ~ bart(. -z -group_id) + (1|group_id),
                               bart_args = list(keepTrees = TRUE, n.trees = 100),
                               data = sim$data, warmup = 2000, iter = 7000)

  # update when https://github.com/vdorie/stan4bart/issues/7 fixed
  varmean_long <- stan4bart:::combine_chains_f(
    stan4bart:::get_samples(var_select_bart$bart_varcount, F, T)
    ) %>% rowMeans()

  # change categoricals to just one variable
  c1_id <- grepl("c1", names(varmean_long))
  varmean <- c(list(c1 = sum(varmean_long[c1_id]), varmean_long[!c1_id]), recursive = T)

  # select most important vars from y ~ covariates model
  # very simple selection mechanism. Should use cross-validation in practice
  var_select <- names(varmean)[varmean > 9.9]

  # regress z ~ most important covariates to get propensity score
  # BART::pbart is for probit regression
  prop_bart <- stan4bart(z ~ bart(. -group_id) + (1|group_id),
                         bart_args = list(keepTrees = TRUE, n.trees = 100),
                         data = sim$data[,c("z", "group_id", var_select)],
                         warmup = 2000, iter = 7000)


  sim$data$prop_score <- fitted(prop_bart)


  bmodel <- stan4bart(y ~ bart(. -group_id) + (1|group_id),
                      bart_args = list(keepTrees = TRUE, n.trees = 100),
                      data = sim$data, warmup = 2000, iter = 7000)

})

suhillsim_ranef1 <- sim
stan4bartmodel1 <- bmodel

usethis::use_data(suhillsim_ranef1, overwrite = T)
usethis::use_data(stan4bartmodel1, overwrite = T)
