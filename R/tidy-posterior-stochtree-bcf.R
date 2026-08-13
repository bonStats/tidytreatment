#' Get fitted draws from posterior of \code{stochtree}-package BCF (causal forest) models
#'
#' Fitted values here are the combined outcome prediction \code{y_hat = mu(X) + tau(X)*Z}
#' (plus any random effects). \code{stochtree::predict.bcfmodel()}/\code{extractParameter()}
#' already fold in the treatment-effect intercept and adaptive-coding parameters (if used) into
#' \code{tau_hat}/\code{y_hat}, so no extra recombination is needed here.
#'
#' @param model A \code{bcfmodel} from the \code{stochtree} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param treatment Treatment assignment vector for \code{newdata} (required if \code{newdata} is supplied).
#' @param propensity Propensity score vector for \code{newdata}, if the model requires one (required if \code{newdata} is supplied and the model used a propensity score covariate).
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real (link) or probability scale?
#' @param ... Arguments to pass to \code{predict} (e.g. \code{stochtree:::predict.bcfmodel}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_stochtree_bcf <- function(model, newdata = NULL, treatment = NULL, propensity = NULL, value = ".value", ..., include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  stopifnot(has_installed_package("stochtree"))

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    inherits(model, "bcfmodel"),
    model$model_params$outcome_model$link %in% c('identity', 'probit', 'cloglog') # no other links implemented
  )

  if (is.null(newdata) & include_newdata) {
    stop("For models from stochtree package 'newdata'
          must be specified if 'include_newdata = TRUE'.")
  }

  use_scale <- match.arg(scale,
    c("real", "prob"),
    several.ok = F
  )

  link <- model$model_params$outcome_model$link
  needs_transform <- use_scale == "prob" && link != "identity"

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  if (!(missing(newdata) | is.null(newdata))) {
    if (is.null(treatment)) {
      stop("`treatment` (a vector of treatment assignments for `newdata`) must be supplied to predict on new data for bcfmodel objects.")
    }
    if (nrow(newdata) != length(treatment)) {
      stop("`treatment` must have one value per row of `newdata`.")
    }

    predict_scale <- if (needs_transform) "probability" else "linear"
    posterior <- predict(model, X = newdata, Z = treatment, propensity = propensity, terms = "y_hat", scale = predict_scale, ...)
  } else {
    # extractParameter() has no scale argument: only ever the linear scale.
    posterior <- stochtree::extractParameter(model, term = "y_hat_train")

    if (needs_transform) {
      # base R link functions cover all outcome-model links stochtree supports here
      posterior <- stats::make.link(link)$linkinv(posterior)
    }
  }

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if (include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(posterior, .name_repair = function(names) {
      paste0(".col_iter", as.character(1:length(names)))
    }),
    .row = 1:nrow(posterior)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = !!value, dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer(gsub(pattern = ".col_iter", replacement = "", x = .data$.draw)))

  # include sigma^2 if needed
  if (include_sigsqs) {
    sigsq <- dplyr::bind_cols(
      .draw = 1:length(model$sigma2_global_samples),
      sigsq = model$sigma2_global_samples
    )

    out <- dplyr::left_join(out, sigsq, by = ".draw")

    col_order <- c(col_order, "sigsq")
  }

  # rearrange
  out <- dplyr::select(out, -!!col_order, !!col_order)

  # group
  row_groups <- names(out)[!names(out) %in% col_order[col_order != ".row"]]

  out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(row_groups)))

  return(out)
}

#' Get fitted draws from posterior of \code{bcfmodel} (\code{stochtree} package) model
#'
#' @inheritParams fitted_draws_stochtree_bcf
#' @param object A \code{bcfmodel} from the \code{stochtree} package.
#' @param ndraws Not currently implemented.
#' @param ... Additional arguments passed to \code{predict.bcfmodel}.
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.bcfmodel <- function(object, newdata, treatment = NULL, propensity = NULL, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted_draws_stochtree_bcf(
    model = object, newdata = newdata, treatment = treatment, propensity = propensity, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    scale = scale
  )
}

#' Get linear predictor draws from posterior of \code{bcfmodel} (\code{stochtree} package) model
#'
#' Typically referred to as fitted value draws on the linear (pre-link) scale, i.e. before any
#' probit/cloglog transform of the combined outcome prediction is applied.
#'
#' @inheritParams epred_draws.bcfmodel
#' @param value The name of the output column for \code{linpred_draws}; default \code{".linpred"}.
#'
#' @return A tidy data frame (tibble) with linear predictor values.
#' @export
#'
linpred_draws.bcfmodel <- function(object, newdata, treatment = NULL, propensity = NULL, value = ".linpred", ..., ndraws = NULL, include_newdata = TRUE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  epred_draws.bcfmodel(
    object = object, newdata = newdata, treatment = treatment, propensity = propensity, value = value,
    ...,
    ndraws = ndraws,
    include_newdata = include_newdata,
    scale = "real"
  )
}

#' Get predict draws from posterior of \code{bcfmodel} (\code{stochtree} package) model
#'
#' Supports continuous outcome models (draws from \code{Normal(fitted, sigma^2)}) and binary
#' outcome models (draws from \code{Bernoulli(fitted probability)}), same as \code{predicted_draws.bartmodel}.
#'
#' @inheritParams epred_draws.bcfmodel
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.bcfmodel <- function(object, newdata, treatment = NULL, propensity = NULL, value = ".prediction", ..., ndraws = NULL, include_newdata = TRUE, include_fitted = FALSE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  outcome <- object$model_params$outcome_model$outcome

  if (outcome == "continuous") {
    out <- fitted_draws_stochtree_bcf(
      model = object, newdata = newdata, treatment = treatment, propensity = propensity, value = ".fit",
      ...,
      include_newdata = include_newdata,
      include_sigsqs = TRUE,
      scale = "real"
    )

    out <- dplyr::mutate(out, !!value := stats::rnorm(n = dplyr::n(), mean = .data$.fit, sd = sqrt(.data$sigsq)))

    if (!include_sigsqs) out <- dplyr::select(out, -"sigsq")
    if (!include_fitted) out <- dplyr::select(out, -".fit")
  } else if (outcome == "binary") {
    out <- fitted_draws_stochtree_bcf(
      model = object, newdata = newdata, treatment = treatment, propensity = propensity, value = ".fitted",
      ...,
      include_newdata = include_newdata,
      include_sigsqs = FALSE,
      scale = "prob"
    )

    out <- dplyr::mutate(out, !!value := stats::rbinom(dplyr::n(), 1, .data$.fitted))

    if (!include_fitted) out <- dplyr::select(out, -".fitted")
  } else {
    stop("predicted_draws is not implemented for bcf outcome type '", outcome, "'.")
  }

  return(out)
}

#' Get residual draw for \code{bcfmodel} (\code{stochtree} package) model
#'
#' The original response variable must be passed as an argument to this function.
#' e.g. \code{response = y}
#'
#' @inheritParams epred_draws.bcfmodel
#' @param response Original response vector.
#' @param value Name of the output column for residual_draws; default is \code{.residual}.
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.bcfmodel <- function(object, newdata, treatment = NULL, propensity = NULL, response, value = ".residual", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(response)) stop("Models from stochtree package require response (y) as argument. Specify 'response = <y variable>' as argument.")

  stopifnot(is.numeric(response))

  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  obs <- dplyr::tibble(y = response, .row = 1:length(response))

  fitted <- epred_draws.bcfmodel(
    object = object, newdata = newdata, treatment = treatment, propensity = propensity,
    value = ".fitted",
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    ...
  )

  out <- dplyr::mutate(
    dplyr::left_join(fitted, obs, by = ".row"),
    !!value := .data$y - .data$.fitted
  )

  dplyr::group_by(out, .data$.row)
}

#' Tidy access to posterior of \code{bcfmodel} (\code{stochtree} package) model
#'
#' Returns the per-draw global scalar parameters actually sampled by the model (e.g.
#' \code{sigma2_global}, \code{sigma2_leaf_mu}, \code{sigma2_leaf_tau}, \code{tau_0}, adaptive
#' coding parameters \code{b_0}/\code{b_1}), whichever apply to this particular model.
#'
#' @param model A \code{bcfmodel} from the \code{stochtree} package.
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) of posterior draws.
#' @export
#'
tidy_draws.bcfmodel <- function(model, ...) {
  stopifnot(inherits(model, "bcfmodel"))

  n_samples <- model$model_params$num_samples

  out <- dplyr::tibble(
    .chain = NA_integer_,
    .iteration = NA_integer_,
    .draw = seq_len(n_samples)
  )

  if (isTRUE(model$model_params$sample_sigma2_global)) {
    out$sigma2_global <- model$sigma2_global_samples
  }

  if (isTRUE(model$model_params$sample_sigma2_leaf_mu)) {
    out$sigma2_leaf_mu <- model$sigma2_leaf_mu_samples
  }

  if (isTRUE(model$model_params$sample_sigma2_leaf_tau)) {
    out$sigma2_leaf_tau <- model$sigma2_leaf_tau_samples
  }

  if (isTRUE(model$model_params$sample_tau_0)) {
    out$tau_0 <- as.vector(model$tau_0_samples)
  }

  if (isTRUE(model$model_params$adaptive_coding)) {
    out$b_0 <- as.vector(model$b_0_samples)
    out$b_1 <- as.vector(model$b_1_samples)
  }

  return(out)
}

#' @export
covariate_importance.bcfmodel <- function(model, X_train, forest = c("treatment_effect", "prognostic"), ...) {

  stopifnot("X_train used to fit the model must be provided for stochtree package" = !missing(X_train))

  forest <- match.arg(forest)

  base_vars <- colnames(X_train)[model$train_set_metadata$original_var_indices]

  # the prognostic and treatment-effect forests can have different covariate
  # sets: e.g. by default the propensity score is added as an extra covariate
  # to the prognostic forest only (model_params$propensity_covariate), so
  # num_prognostic_covariates != num_treatment_covariates in that case
  if (forest == "treatment_effect") {
    forest_obj <- model$forests_tau
    p <- model$model_params$num_treatment_covariates
    has_propensity <- model$model_params$propensity_covariate %in% c("treatment_effect", "both")
  } else {
    forest_obj <- model$forests_mu
    p <- model$model_params$num_prognostic_covariates
    has_propensity <- model$model_params$propensity_covariate %in% c("prognostic", "both")
  }

  variable_names <- if (has_propensity) c(base_vars, "propensity") else base_vars
  stopifnot(length(variable_names) == p)

  res <- dplyr::tibble(
    variable = variable_names,
    inclusion = forest_obj$get_aggregate_split_counts(p)
  )

  res |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(inclusion = sum(.data$inclusion)) |>
    dplyr::mutate(avg_inclusion = .data$inclusion / sum(.data$inclusion)) |>
    dplyr::select(-"inclusion")
}

#' Get (individual) treatment effect draws from posterior of a \code{bcfmodel} (\code{stochtree} package)
#'
#' Unlike \code{treatment_effects.default}, this does not compute counterfactuals by flipping
#' \code{treatment} and taking two \code{epred_draws()} calls: \code{stochtree::bcf()} already
#' fits a dedicated treatment-effect forest, and \code{tau_hat}/\code{predict(..., terms = "tau")}
#' already fold in the treatment-effect intercept and adaptive-coding parameters (if used), so the
#' causal estimate is used directly.
#'
#' Note that, unlike \code{treatment_effects.bartcFit} (which forbids a \code{treatment} argument
#' because a \code{bartcFit} object stores its own training data and treatment vector),
#' \code{bcfmodel} objects do not store their training \code{X}/\code{Z}, so \code{treatment} here
#' must be supplied as the raw treatment vector itself (not a column name string, as in
#' \code{treatment_effects.default}) whenever \code{newdata} is given or \code{subset != "all"}.
#'
#' @param model A \code{bcfmodel} from the \code{stochtree} package.
#' @param treatment A vector of treatment assignments. Required if \code{newdata} is supplied (to predict on it) or if \code{subset != "all"} (to filter by it). Not a column name string.
#' @param newdata Data frame to generate treatment effect draws from. If omitted, uses the in-sample \code{tau_hat_train} already computed by \code{bcf()}.
#' @param subset Either "treated", "nontreated", or "all". Default is "all".
#' @param common_support_method Not currently supported for \code{bcfmodel} objects; a warning is issued if supplied.
#' @param cutoff Not currently supported for \code{bcfmodel} objects.
#' @param propensity Propensity score vector for \code{newdata}, if the model requires one.
#' @param ... Arguments to be passed to \code{predict.bcfmodel} (e.g. scale-related arguments are not exposed here).
#'
#' @return A tidy data frame (tibble) with treatment effect values in the \code{cte} column.
#' @export
#'
treatment_effects.bcfmodel <- function(model, treatment = NULL, newdata = NULL, subset = "all", common_support_method, cutoff, propensity = NULL, ...) {

  if (!missing(common_support_method)) {
    warning("`common_support_method` is not currently supported for bcfmodel objects and will be ignored.")
  }

  use_subset <- match.arg(subset, c("all", "treated", "nontreated"))

  if (is.null(newdata)) {
    posterior <- stochtree::extractParameter(model, term = "tau_hat_train")
  } else {
    if (is.null(treatment)) {
      stop("`treatment` (a vector of treatment assignments for `newdata`) must be supplied to predict on new data for bcfmodel objects.")
    }
    if (nrow(newdata) != length(treatment)) {
      stop("`treatment` must have one value per row of `newdata`.")
    }

    posterior <- predict(model, X = newdata, Z = treatment, propensity = propensity, terms = "tau", scale = "linear", ...)
  }

  out <- dplyr::bind_cols(
    dplyr::as_tibble(posterior, .name_repair = function(names) {
      paste0(".col_iter", as.character(1:length(names)))
    }),
    .row = 1:nrow(posterior)
  )

  out <- tidyr::gather(out, key = ".draw", value = "cte", dplyr::starts_with(".col_iter"))

  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer(gsub(pattern = ".col_iter", replacement = "", x = .data$.draw)))

  out <- dplyr::select(out, ".row", ".chain", ".iteration", ".draw", "cte")

  if (use_subset != "all") {
    if (is.null(treatment)) {
      stop("`treatment` (a vector of treatment assignments) must be supplied to filter by subset.")
    }

    treated_rows <- is_treated(treatment)
    keep_rows <- if (use_subset == "treated") which(treated_rows) else which(!treated_rows)
    out <- dplyr::filter(out, .data$.row %in% keep_rows)
  }

  dplyr::group_by(out, .data$.row)
}
