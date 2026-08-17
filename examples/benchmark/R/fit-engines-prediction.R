# Part A (vanilla prediction) fitting adapters: one function per
# engine/variant row, sharing the hyperparameter adapters in hyperparams.R.
# Each returns the raw fitted model object - extraction goes through
# tidytreatment's own epred_draws() elsewhere (extract-tidy.R), not here.

fit_wbart <- function(X, y, hp) {
  args <- hp_to_wbart(hp)
  do.call(BART::wbart, c(list(x.train = X, y.train = y, printevery = 10000L), args))
}

fit_pbart <- function(X, y, hp) {
  args <- hp_to_pbart(hp)
  do.call(BART::pbart, c(list(x.train = X, y.train = as.integer(y), printevery = 10000L), args))
}

# stan4bart is NOT included in vanilla (no-groups) prediction: it's
# architecturally a mixed-effects+BART model (its glFormula parser requires
# at least one (1|group) term and errors on a pure bart(.)-only formula), so
# it has no fair "no groups" mode. It's tested separately, on its own natural
# ground, in the random-effects prediction document
# (fit-engines-rfx.R / benchmark-prediction-rfx.qmd). dbarts::bart2() has no
# such restriction, so it fills the "plain BART, no groups" slot here instead.

fit_dbarts_bart <- function(X, y, hp, outcome = c("continuous", "binary")) {
  outcome <- match.arg(outcome)
  args <- hp_to_dbarts(hp)
  y_train <- if (outcome == "binary") as.integer(y) else y
  dat <- cbind(X, data.frame(.y = y_train))
  do.call(dbarts::bart2, c(list(.y ~ ., data = dat), args))
}

fit_stochtree_bart <- function(X, y, hp, outcome = c("continuous", "binary"), num_gfr = 0) {
  outcome <- match.arg(outcome)
  args <- hp_to_stochtree_bart(hp, y = y, outcome = outcome, num_gfr = num_gfr)
  y_train <- if (outcome == "binary") as.integer(y) else y
  # X_train must be a data.frame, not a matrix: stochtree::bart() silently
  # discards a matrix's real colnames and relabels columns positionally as
  # x1, x2, ... internally, which then fails to match newdata's real column
  # names at predict() time (see fit-engines-causal.R for the failure this
  # caused and how it was diagnosed).
  do.call(stochtree::bart, c(list(X_train = X, y_train = y_train), args))
}
