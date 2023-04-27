## Code to prepare `suhillsim2_ranef` and `stan4bartmodel3`
library(tidytreatment)
library(dbarts)
library(stan4bart)
library(bartCause)
library(dplyr)
withr::with_seed(101, {
  sim <- simulate_su_hill_data(
    n = 100, treatment_linear = F, omega = 0, add_categorical = T,
    n_subjects = 10,
    sd_subjects = 2,
    coef_categorical_treatment = c(0, 0, 1),
    coef_categorical_nontreatment = c(-1, 0, -1)
  )

  dat <- sim$data

  # STEP 1 VS Model: Regress y ~ covariates
  vs_bart <- stan4bart(y ~ bart(. - subject_id - z) + (1|subject_id),
                       data = dat, iter = 5000, verbose = -1)

  # STEP 2: Variable selection
  # select most important vars from y ~ covariates model
  # very simple selection mechanism. Should use cross-validation in practice
  covar_ranking <- covariate_importance(vs_bart)
  var_select <- covar_ranking %>%
    filter(avg_inclusion > mean(avg_inclusion) - sd(avg_inclusion)) %>% # at minimum: within 1 sd of mean inclusion
    pull(variable)

  # change categorical variables to just one variable
  var_select <- unique(gsub("c1.[1-3]$","c1", var_select))

  var_select
  # includes all covariates

  # STEP 3 PS Model: Regress z ~ selected covariates
  ps_bart <- stan4bart(z ~ bart(. - subject_id - y) + (1|subject_id),
                       data = dat, iter = 5000, verbose = -1)

  # store propensity score in data
  prop_score <- fitted(ps_bart)

  # Step 4 TE Model: Regress y ~ z + covariates + propensity score
  te_bart <- bartc(response = y, treatment = z,
                   confounders = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10,
                   parametric = (1|subject_id), data = dat, method.trt = prop_score,
                   warmup = 5000, iter = 6000, chains = 1, bart_args = list(keepTrees = FALSE, keepTrainingFits = FALSE))



})

suhillsim2_ranef <- sim
stan4bartmodel2 <- te_bart

usethis::use_data(suhillsim2_ranef, overwrite = T)
# usethis::use_data(stan4bartmodel2, overwrite = T) # too large
