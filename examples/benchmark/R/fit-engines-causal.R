# Part B (causal estimates) fitting adapters.
#
# stan4bart is not included as a standalone two-step causal row, for the same
# reason it's excluded from Part A (requires a (1|group) term, and these
# DGPs have no grouping structure). fit_diy_propensity() standardizes on
# BART::pbart as the one propensity engine for every DIY row, so the
# propensity step doesn't vary alongside the causal engine being compared.
#
# Every *_twostep() fitter returns list(fit=, newdata=, treatment_col=) -
# newdata always includes the treatment column (named ".z") plus the
# propensity score used, so treatment_effects()/avg_treatment_effects() can
# be called uniformly downstream regardless of engine.

fit_bart_twostep <- function(X, y, z, hp, propensity_recipe = c("two_stage", "ps_all")) {
  propensity_recipe <- match.arg(propensity_recipe)
  diy <- fit_diy_propensity(X, y, z, hp, recipe = propensity_recipe)

  newdata <- cbind(X, data.frame(.z = z, .prop = diy$propensity))
  args <- hp_to_wbart(hp)
  fit <- do.call(BART::wbart, c(list(x.train = newdata, y.train = y, printevery = 10000L), args))

  list(fit = fit, newdata = newdata, treatment_col = ".z", propensity = diy$propensity, selected_vars = diy$selected_vars)
}

fit_pbart_twostep <- function(X, y, z, hp, propensity_recipe = c("two_stage", "ps_all")) {
  propensity_recipe <- match.arg(propensity_recipe)
  diy <- fit_diy_propensity(X, y, z, hp, recipe = propensity_recipe)

  newdata <- cbind(X, data.frame(.z = z, .prop = diy$propensity))
  args <- hp_to_pbart(hp)
  fit <- do.call(BART::pbart, c(list(x.train = newdata, y.train = as.integer(y), printevery = 10000L), args))

  list(fit = fit, newdata = newdata, treatment_col = ".z", propensity = diy$propensity, selected_vars = diy$selected_vars)
}

fit_stochtree_bart_twostep <- function(X, y, z, hp, propensity_recipe = c("two_stage", "ps_all"),
                                        outcome = c("continuous", "binary"), num_gfr = 0) {
  propensity_recipe <- match.arg(propensity_recipe)
  outcome <- match.arg(outcome)
  diy <- fit_diy_propensity(X, y, z, hp, recipe = propensity_recipe)

  newdata <- cbind(X, data.frame(.z = z, .prop = diy$propensity))
  args <- hp_to_stochtree_bart(hp, X = newdata, y = y, outcome = outcome, num_gfr = num_gfr)
  y_train <- if (outcome == "binary") as.integer(y) else y
  # X_train must be a data.frame, not a matrix - see fit-engines-prediction.R
  fit <- do.call(stochtree::bart, c(list(X_train = newdata, y_train = y_train), args))

  list(fit = fit, newdata = newdata, treatment_col = ".z", propensity = diy$propensity, selected_vars = diy$selected_vars)
}

# propensity_mode: "diy_two_stage" / "diy_ps_all" (method.trt = a precomputed
# score vector, from fit_diy_propensity()) or "builtin" (method.trt = "bart",
# bartCause's own internal treatment model).
fit_bartc <- function(X, y, z, hp, propensity_mode = c("diy_two_stage", "diy_ps_all", "builtin"), auto_k = FALSE) {
  propensity_mode <- match.arg(propensity_mode)
  dat <- cbind(X, data.frame(.y = y, .z = z))
  # bartc()'s `confounders` uses NSE - a bare sum expression (x1 + x2 + x3),
  # not a formula object; stats::reformulate() fails inside bartc()'s own terms.formula() call.
  confounders_expr <- str2lang(paste(colnames(X), collapse = " + "))

  method_trt <- if (propensity_mode == "builtin") {
    "bart"
  } else {
    recipe <- if (propensity_mode == "diy_two_stage") "two_stage" else "ps_all"
    fit_diy_propensity(X, y, z, hp, recipe = recipe)$propensity
  }

  args_rsp <- hp_to_dbarts(hp, auto_k = auto_k)
  args_trt <- hp_to_dbarts(hp, auto_k = auto_k)

  # bartc() breaks under do.call() ("wrong arguments for subsetting an
  # environment", from its NSE handling of `data`) - built via bquote()/eval() instead.
  call_expr <- bquote(bartCause::bartc(
    response = .y, treatment = .z, confounders = .(confounders_expr),
    data = dat, method.rsp = "bart", method.trt = .(method_trt),
    args.rsp = .(args_rsp), args.trt = .(args_trt),
    seed = NA_integer_, verbose = FALSE
  ))
  eval(call_expr)
}

# propensity_mode: "diy_two_stage" / "diy_ps_all" (propensity_train = a
# precomputed score vector) or "builtin" (propensity_train = NULL, bcf's own
# internal stochtree::bart()-based propensity estimation).
fit_bcf <- function(X, y, z, hp, propensity_mode = c("diy_two_stage", "diy_ps_all", "builtin"),
                     outcome = c("continuous", "binary"), num_gfr = 0, adaptive_coding = FALSE) {
  propensity_mode <- match.arg(propensity_mode)
  outcome <- match.arg(outcome)

  propensity <- if (propensity_mode == "builtin") {
    NULL
  } else {
    recipe <- if (propensity_mode == "diy_two_stage") "two_stage" else "ps_all"
    fit_diy_propensity(X, y, z, hp, recipe = recipe)$propensity
  }

  args <- hp_to_stochtree_bcf(hp, X = X, y = y, outcome = outcome, num_gfr = num_gfr, adaptive_coding = adaptive_coding)
  y_train <- if (outcome == "binary") as.integer(y) else y

  # X_train must be a data.frame, not a matrix - see fit-engines-prediction.R
  do.call(stochtree::bcf, c(
    list(X_train = X, Z_train = z, y_train = y_train, propensity_train = propensity),
    args
  ))
}
