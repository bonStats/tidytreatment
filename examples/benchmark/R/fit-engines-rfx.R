# Random-effects prediction fitting adapters. stan4bart requires a literal
# formula, not one built via as.formula(paste(...)).

fit_stan4bart_rfx <- function(X, y, group, hp, auto_k = FALSE) {
  .y <- y
  .group <- group
  dat <- cbind(X, data.frame(.y = .y, .group = .group))
  dbart_args <- hp_to_dbarts(hp, auto_k = auto_k)

  stan4bart::stan4bart(
    .y ~ bart(. - .group) + (1 | .group),
    data = dat,
    verbose = -1,
    iter = hp$draws + hp$burn,
    warmup = hp$burn,
    chains = hp$chains,
    bart_args = dbart_args
  )
}

# Intercept-only random effects - the direct analogue of stan4bart's
# (1|group). stochtree's random_effects_params$model_spec actually defaults
# to "custom" (which then requires an explicit rfx_basis_train and errors
# without one) rather than "intercept_only", so this must be requested
# explicitly.
fit_stochtree_bart_rfx <- function(X, y, group, hp, outcome = c("continuous", "binary"), num_gfr = 0) {
  outcome <- match.arg(outcome)
  args <- hp_to_stochtree_bart(hp, X = X, y = y, outcome = outcome, num_gfr = num_gfr)
  args$random_effects_params <- list(model_spec = "intercept_only")
  y_train <- if (outcome == "binary") as.integer(y) else y

  do.call(stochtree::bart, c(
    list(X_train = X, y_train = y_train, rfx_group_ids_train = as.integer(group)),
    args
  ))
}
