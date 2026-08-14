#' Get fitted draws from posterior of \code{stochtree}-package models
#'
#' @param model A model from \code{stochtree} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param rfx_group_ids Random effect group labels for \code{newdata} (required if the model was fit with random effects and \code{newdata} is supplied).
#' @param rfx_basis Random effect basis for \code{newdata} (required if the model was fit with a \code{"custom"} random effects basis and \code{newdata} is supplied; optional/ignored otherwise).
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real (link) or probability scale?
#' @param ... Arguments to pass to \code{predict} (e.g. \code{stochtree:::predict.bartmodel}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_stochtree <- function(model, newdata = NULL, rfx_group_ids = NULL, rfx_basis = NULL, value = ".value", ..., include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  stopifnot(has_installed_package("stochtree"))

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    inherits(model, "bartmodel"),
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
  # scale = "probability" is only a real transform for probit/cloglog outcome
  # models; predict.bartmodel() actively errors if requested for an identity
  # link, and it's a no-op for identity anyway (matches the wbart precedent,
  # where scale = "prob" is a harmless no-op for continuous-outcome models).
  needs_transform <- use_scale == "prob" && link != "identity"

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  if (!(missing(newdata) | is.null(newdata))) {
    stochtree_check_rfx_args(model, rfx_group_ids, rfx_basis)

    # predict.bartmodel() applies the outcome model's own probability-scale
    # transform internally, so prefer that over reimplementing it ourselves.
    # It also folds in the random effects contribution (confirmed empirically:
    # y_hat_train == mean_forest(X) + rfx_preds_train, exactly), so no manual
    # recombination is needed here either.
    predict_scale <- if (needs_transform) "probability" else "linear"
    posterior <- predict(model, X = newdata, terms = "y_hat", scale = predict_scale, rfx_group_ids = rfx_group_ids, rfx_basis = rfx_basis, ...)
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

#' Get fitted draws from posterior of \code{bartmodel} (\code{stochtree} package) model
#'
#' @param object A \code{bartmodel} model from the \code{stochtree} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param rfx_group_ids Random effect group labels for \code{newdata} (required if the model was fit with random effects and \code{newdata} is supplied).
#' @param rfx_basis Random effect basis for \code{newdata} (required if the model was fit with a \code{"custom"} random effects basis and \code{newdata} is supplied; optional/ignored otherwise).
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real (link) or probability scale?
#' @param ... Additional arguments passed to \code{predict.bartmodel}.
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.bartmodel <- function(object, newdata, rfx_group_ids = NULL, rfx_basis = NULL, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted_draws_stochtree(
    model = object, newdata = newdata, rfx_group_ids = rfx_group_ids, rfx_basis = rfx_basis, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    scale = scale
  )
}

#' Get linear predictor draws from posterior of \code{bartmodel} (\code{stochtree} package) model
#'
#' Typically referred to as fitted value draws on the linear (pre-link) scale. For a continuous
#' outcome model (identity link) this is identical to \code{epred_draws}; for a binary outcome
#' model it is the latent/probit-scale value, before the link function is applied.
#'
#' @inheritParams epred_draws.bartmodel
#' @param value The name of the output column for \code{linpred_draws}; default \code{".linpred"}.
#'
#' @return A tidy data frame (tibble) with linear predictor values.
#' @export
#'
linpred_draws.bartmodel <- function(object, newdata, rfx_group_ids = NULL, rfx_basis = NULL, value = ".linpred", ..., ndraws = NULL, include_newdata = TRUE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  epred_draws.bartmodel(
    object = object, newdata = newdata, rfx_group_ids = rfx_group_ids, rfx_basis = rfx_basis, value = value,
    ...,
    ndraws = ndraws,
    include_newdata = include_newdata,
    scale = "real"
  )
}

#' Get predict draws from posterior of \code{bartmodel} (\code{stochtree} package) model
#'
#' Supports continuous outcome models (draws from \code{Normal(fitted, sigma^2)}) and binary
#' outcome models (draws from \code{Bernoulli(fitted probability)}).
#'
#' @param object A \code{bartmodel} model from the \code{stochtree} package.
#' @param newdata Data frame to generate predictions from. If omitted, predictions are generated from the data used to fit the model.
#' @param rfx_group_ids Random effect group labels for \code{newdata} (required if the model was fit with random effects and \code{newdata} is supplied).
#' @param rfx_basis Random effect basis for \code{newdata} (required if the model was fit with a \code{"custom"} random effects basis and \code{newdata} is supplied; optional/ignored otherwise).
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included? Only applicable to continuous outcome models.
#' @param ... Additional arguments passed to \code{predict.bartmodel}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.bartmodel <- function(object, newdata, rfx_group_ids = NULL, rfx_basis = NULL, value = ".prediction", ..., ndraws = NULL, include_newdata = TRUE, include_fitted = FALSE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  outcome <- object$model_params$outcome_model$outcome

  if (outcome == "continuous") {
    out <- fitted_draws_stochtree(
      model = object, newdata = newdata, rfx_group_ids = rfx_group_ids, rfx_basis = rfx_basis, value = ".fit",
      ...,
      include_newdata = include_newdata,
      include_sigsqs = TRUE,
      scale = "real"
    )

    out <- dplyr::mutate(out, !!value := stats::rnorm(n = dplyr::n(), mean = .data$.fit, sd = sqrt(.data$sigsq)))

    if (!include_sigsqs) out <- dplyr::select(out, -"sigsq")
    if (!include_fitted) out <- dplyr::select(out, -".fit")
  } else if (outcome == "binary") {
    out <- fitted_draws_stochtree(
      model = object, newdata = newdata, rfx_group_ids = rfx_group_ids, rfx_basis = rfx_basis, value = ".fitted",
      ...,
      include_newdata = include_newdata,
      include_sigsqs = FALSE,
      scale = "prob"
    )

    out <- dplyr::mutate(out, !!value := stats::rbinom(dplyr::n(), 1, .data$.fitted))

    if (!include_fitted) out <- dplyr::select(out, -".fitted")
  } else {
    stop("predicted_draws is not implemented for stochtree outcome type '", outcome, "'.")
  }

  return(out)
}

#' Get residual draw for \code{bartmodel} (\code{stochtree} package) model
#'
#' The original response variable must be passed as an argument to this function.
#' e.g. \code{response = y}
#'
#' @param object A \code{bartmodel} model from the \code{stochtree} package.
#' @param newdata Data frame to generate predictions from. If omitted, original data used to fit the model.
#' @param rfx_group_ids Random effect group labels for \code{newdata} (required if the model was fit with random effects and \code{newdata} is supplied).
#' @param rfx_basis Random effect basis for \code{newdata} (required if the model was fit with a \code{"custom"} random effects basis and \code{newdata} is supplied; optional/ignored otherwise).
#' @param response Original response vector.
#' @param value Name of the output column for residual_draws; default is \code{.residual}.
#' @param ... Additional arguments passed to \code{predict.bartmodel}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.bartmodel <- function(object, newdata, rfx_group_ids = NULL, rfx_basis = NULL, response, value = ".residual", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(response)) stop("Models from stochtree package require response (y) as argument. Specify 'response = <y variable>' as argument.")

  stopifnot(is.numeric(response))

  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  obs <- dplyr::tibble(y = response, .row = 1:length(response))

  fitted <- epred_draws.bartmodel(
    object = object, newdata = newdata, rfx_group_ids = rfx_group_ids, rfx_basis = rfx_basis,
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

#' Tidy access to posterior of \code{bartmodel} (\code{stochtree} package) model
#'
#' Returns the per-draw global scalar parameters sampled by the model (e.g.
#' \code{sigma2_global}, \code{sigma2_leaf}), whichever were actually sampled.
#'
#' @param model A \code{bartmodel} model from the \code{stochtree} package.
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) of posterior draws.
#' @export
#'
tidy_draws.bartmodel <- function(model, ...) {
  stopifnot(inherits(model, "bartmodel"))

  n_samples <- model$model_params$num_samples

  out <- dplyr::tibble(
    .chain = NA_integer_,
    .iteration = NA_integer_,
    .draw = seq_len(n_samples)
  )

  if (isTRUE(model$model_params$sample_sigma2_global)) {
    out$sigma2_global <- model$sigma2_global_samples
  }

  if (isTRUE(model$model_params$sample_sigma2_leaf)) {
    out$sigma2_leaf <- model$sigma2_leaf_samples
  }

  return(out)
}
