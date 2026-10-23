# Random-effects prediction fitting adapters. stan4bart requires a literal
# formula, not one built via as.formula(paste(...)).

fit_stan4bart_rfx <- function(X, y, group, hp, variant = c("baseline", "default", "default+sigest"), seed = NA_integer_) {
  variant <- match.arg(variant)
  .y <- y
  .group <- group
  dat <- cbind(X, data.frame(.y = .y, .group = .group))

  # "default" and "default+sigest" both leave bart_args entirely at
  # stan4bart's own out-of-the-box tree/leaf-prior settings (via
  # dbartsControl/parsePriors' own defaults) - burn/draws/chains still come
  # from hp, same MCMC-budget-only principle as the other engines' "default"
  # rows. Only "baseline" matches tree structure to hp too.
  #
  # k comes from hp$k (= 2, baseline_hyperparams()'s default, same as the
  # vanilla prediction benchmark) unconditionally - no auto_k variant here.
  # Investigating stan4bart's own auto_k row showed it doesn't do what its
  # name suggests: no k-related parameter is ever sampled (checked directly
  # via the Stan parameter names) for either outcome type, unlike plain
  # dbarts::bart2() where k = NULL is a genuine adaptive chi(1.25, Inf)
  # hyperprior for a binary outcome. Since it produces no real second
  # variant here, it's dropped rather than kept as a misleading row name.
  bart_args <- if (variant == "baseline") hp_to_dbarts(hp) else NULL
  # group is passed so sigest is computed net of the group intercepts rather
  # than absorbing them into the residual (see compute_sigest()).
  #
  # "default+sigest" isolates the prior *family* from the calibration
  # *input*: it keeps stan4bart's own default choice of family
  # (exponential), like "default", but recalibrates its rate from sigest
  # (the same regression-based estimate "baseline" uses) instead of sd(y)
  # (see hp_to_stan4bart_sigma_prior_exp_sigest()'s header comment).
  stan_args <- switch(variant,
    baseline = hp_to_stan4bart_sigma_prior(hp, X = X, y = .y, group = .group),
    "default+sigest" = hp_to_stan4bart_sigma_prior_exp_sigest(hp, X = X, y = .y, group = .group),
    default = NULL
  )

  stan4bart::stan4bart(
    .y ~ bart(. - .group) + (1 | .group),
    data = dat,
    verbose = -1,
    iter = hp$draws + hp$burn,
    warmup = hp$burn,
    chains = hp$chains,
    bart_args = bart_args,
    stan_args = stan_args,
    seed = seed
  )
}

# Intercept-only random effects - the direct analogue of stan4bart's
# (1|group). stochtree's random_effects_params$model_spec actually defaults
# to "custom" (which then requires an explicit rfx_basis_train and errors
# without one) rather than "intercept_only", so this must be requested
# explicitly.
fit_stochtree_bart_rfx <- function(X, y, group, hp, outcome = c("continuous", "binary"), num_gfr = 0,
                                    variant = c("baseline", "default"), seed = NULL) {
  outcome <- match.arg(outcome)
  variant <- match.arg(variant)
  args <- if (variant == "default") {
    hp_to_stochtree_bart_default(hp, outcome = outcome)
  } else {
    hp_to_stochtree_bart(hp, X = X, y = y, outcome = outcome, num_gfr = num_gfr, group = group)
  }
  args$random_effects_params <- list(model_spec = "intercept_only")
  # stochtree's own default (general_params$random_seed = -1) seeds its
  # internal C++ RNG from hardware entropy, not R's own RNG state - set.seed()
  # around the call has no effect on it. Without this, stochtree fits are not
  # reproducible run-to-run even at a fixed R seed (confirmed empirically).
  if (!is.null(seed)) args$general_params$random_seed <- seed

  y_train <- if (outcome == "binary") as.integer(y) else y

  do.call(stochtree::bart, c(
    list(X_train = X, y_train = y_train, rfx_group_ids_train = as.integer(group)),
    args
  ))
}
