#' Check if a model class has required generic methods for tidytreatment functions.
#'
#' @param model Model to be checked.
#'
#' @return Boolean
#' @export
#'
has_tidytreatment_methods <- function(model) {
  cl <- class(model)[1]
  has_method_str(cl, "epred_draws") && has_method_str(cl, "model.matrix")
}


is_01_integer_vector <- function(x) {
  class(x) == "integer" & all(x %in% c(0, 1))
}

has_installed_package <- function(package) {
  length(find.package(package, quiet = TRUE)) >= 1
}

has_method_str <- function(cl, method) {
  # utils::methods(class=)/utils::getS3method() can only resolve a generic
  # that is reachable (as a plain object) from the calling environment's
  # search path. Generics imported (but not re-exported/attached) by
  # tidytreatment - e.g. tidybayes::epred_draws - are invisible to that
  # lookup once tidytreatment is loaded normally (library()/R CMD check),
  # even though the S3 method itself is correctly registered. Searching the
  # S3 method table of every loaded namespace avoids that dependency on
  # search-path visibility.
  method_name <- paste0(method, ".", cl)
  for (pkg in loadedNamespaces()) {
    s3_table <- tryCatch(
      get(".__S3MethodsTable__.", envir = asNamespace(pkg), inherits = FALSE),
      error = function(e) NULL
    )
    if (!is.null(s3_table) && exists(method_name, envir = s3_table, inherits = FALSE)) {
      return(TRUE)
    }
  }
  FALSE
}

check_method <- function(x, method, helper = "") {
  x_cl <- class(x)
  if (!has_method_str(x_cl, method)) {
    stop("Object of class '", x_cl, "' does not have method '", method, "'.\n", helper, call. = FALSE)
  }
}

# Multinomial BART models ('mbart'/'mbart2') are not supported: their
# underlying representation (per-category tree draws in a nested,
# non-flat treedraws structure; no unified in-sample fitted values for
# 'mbart') is incompatible with the machinery this package uses for every
# other BART-package class, and even a working fix wouldn't extend to
# treatment_effects()/avg_treatment_effects(), which assume a scalar
# continuous/binary response.
stop_mbart_unsupported <- function(generic, model) {
  stop(
    "`", generic, "()` is not supported for multinomial BART models (class '", class(model)[1], "'). ",
    call. = FALSE
  )
}

# Shared validation for stochtree bartmodel/bcfmodel newdata prediction with
# random effects. predict.bartmodel()/predict.bcfmodel() give low-level,
# uninformative errors (or a C++-level fatal check failure) if rfx_group_ids/
# rfx_basis are required but missing, so check proactively.
stochtree_check_rfx_args <- function(model, rfx_group_ids, rfx_basis) {
  if (!isTRUE(model$model_params$has_rfx)) {
    return(invisible(NULL))
  }

  if (is.null(rfx_group_ids)) {
    stop(
      "This model was fit with random effects: `rfx_group_ids` (a vector of random effect ",
      "group labels for `newdata`) must be supplied to predict on new data.",
      call. = FALSE
    )
  }

  if (identical(model$model_params$rfx_model_spec, "custom") && is.null(rfx_basis)) {
    stop(
      "This model was fit with a 'custom' random effects basis: `rfx_basis` must also be ",
      "supplied to predict on new data. (It is optional/ignored for the 'intercept_only' and ",
      "'intercept_plus_treatment' random_effects_params$model_spec options, but required for 'custom'.)",
      call. = FALSE
    )
  }

  invisible(NULL)
}

# Warn when `newdata` is supplied with `include_newdata = TRUE`: newdata's
# columns get repeated once per posterior draw in the long-format output,
# which can be a lot of repeated data for many draws or wide newdata. Only
# call this from a branch where newdata is already known to be present.
warn_include_newdata_repeats <- function(include_newdata) {
  if (isTRUE(include_newdata)) {
    warning(
      "`include_newdata = TRUE`: the columns of `newdata` will be repeated once per posterior ",
      "draw in the returned tibble. Set `include_newdata = FALSE` if you don't need `newdata`'s ",
      "columns attached to the output.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

# bartmodel/bcfmodel objects can represent either a continuous or a binary
# outcome model (model$model_params$outcome_model, e.g. OutcomeModel(outcome
# = "binary", link = "probit")), so a single hardcoded "probability" default
# (as used for pbart/lbart) would be misleading for a continuous outcome
# model. Resolve the response-scale default from the model itself instead.
stochtree_default_scale <- function(model) {
  if (identical(model$model_params$outcome_model$outcome, "binary")) "probability" else "linear"
}
