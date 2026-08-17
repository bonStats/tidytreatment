
# Hack detection of binary outcome bart in dbarts
dbarts_is_binary <- function(model) {
  no_sigma <- length(model$sigma) == 0
  has_binary_offset <- "binaryOffset" %in% names(model)

  if (no_sigma != has_binary_offset) {
    stop("Could not determine whether this `dbarts::bart`/`bart2` model has a continuous or ",
         "binary (probit) outcome: `sigma`/`binaryOffset` disagree on it (`length(sigma) == 0` ",
         "is ", no_sigma, ", `'binaryOffset' %in% names(model)` is ", has_binary_offset, ").")
  }

  no_sigma
}

# dbarts stores yhat.train (and predict(..., type = "bart")) on the linear/
# latent scale for a binary (probit) outcome.
# predict(..., type = "ev") applies pnorm() internally to get the response
# (probability) scale.
dbarts_default_scale <- function(model) {
  if (dbarts_is_binary(model)) "probability" else "linear"
}

# model$yhat.train and predict(..., combineChains = FALSE) are 2D [draws x
# obs] when the model has a single chain, but 3D [chains x draws x obs] for
# n.chains > 1 (dbarts's own default is 4, not 1). tidytreatment doesn't track .chain separately for any
# BART-family model (it's always NA), so this always flattens chains into the
# draws dimension rather than keeping them apart.
combine_dbarts_chains <- function(posterior) {
  d <- dim(posterior)
  if (length(d) == 2) {
    return(posterior)
  }
  stopifnot(length(d) == 3)
  matrix(aperm(posterior, c(2, 1, 3)), nrow = d[1] * d[2], ncol = d[3])
}

# model$sigma is a plain vector (length = n.samples) for a single-chain
# model, but a [chains x samples] matrix for n.chains > 1 - flattened here in
# the same chain-major order combine_dbarts_chains() uses for yhat.train/
# predict(), so the two stay aligned by .draw index in the include_sigsqs
# join below (misaligning them would silently attach the wrong sigma^2 draw
# to each row, not just fail to run).
combine_dbarts_sigma <- function(sigma) {
  if (is.null(dim(sigma))) {
    return(sigma)
  }
  as.vector(t(sigma))
}

#' Get fitted draws from posterior of \code{dbarts}-package \code{bart}/\code{bart2} models
#'
#' @param model A \code{bart}-class model from the \code{dbarts} package (e.g. from \code{dbarts::bart2()}), including the response-stage or treatment-stage sub-model of a \code{bartCause::bartc()} fit made without a \code{parametric} argument.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included? Only applicable to continuous outcome models.
#' @param scale Should the fitted values be on the response ("probability"; for a binary/probit outcome model) or linear predictor ("linear") scale? Accepts unambiguous abbreviations (e.g. \code{"prob"}, \code{"lin"}). Default (\code{NULL}) uses the response scale for the model's outcome type.
#' @param ... Arguments to pass to \code{predict} (e.g. \code{dbarts:::predict.bart}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_dbarts <- function(model, newdata = NULL, value = ".value", ..., include_newdata = FALSE, include_sigsqs = FALSE, scale) {
  stopifnot(has_installed_package("dbarts"))

  if (is.null(newdata) & include_newdata) {
    stop("`newdata` was not supplied, but `include_newdata = TRUE`: `dbarts::bart`/`bart2` models don't ",
         "store their training data in an easily reusable form, so there's nothing to attach to the ",
         "output. Either supply `newdata` explicitly, or set `include_newdata = FALSE` if you don't ",
         "need the fitted data attached.")
  }

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    inherits(model, "bart")
  )

  if (is.null(scale)) scale <- dbarts_default_scale(model)
  use_scale <- match.arg(scale, c("linear", "probability"), several.ok = FALSE)
  predict_type <- if (use_scale == "probability") "ev" else "bart"

  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  if (!(missing(newdata) | is.null(newdata))) {
    posterior <- predict(object = model, newdata = newdata, type = predict_type, combineChains = FALSE, ...)
    posterior <- combine_dbarts_chains(posterior)
  } else if (predict_type == "ev") {
    # yhat.train is always on the linear/latent scale (see dbarts_default_scale());
    # applying the model's own link inverse matches predict(..., type = "ev")
    # exactly for a binary outcome, and is a no-op for a continuous one (no
    # link function - matches the wbart precedent).
    posterior <- combine_dbarts_chains(model$yhat.train)
    if (dbarts_is_binary(model)) posterior <- stats::pnorm(posterior)
  } else {
    posterior <- combine_dbarts_chains(model$yhat.train)
  }

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if (include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(t(posterior), .name_repair = function(names) {
      paste0(".col_iter", as.character(1:length(names)))
    }),
    .row = 1:ncol(posterior)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = !!value, dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer(gsub(pattern = ".col_iter", replacement = "", x = .data$.draw)))

  # include sigma^2 if needed
  if (include_sigsqs) {
    sigma_flat <- combine_dbarts_sigma(model$sigma)
    sigsq <- dplyr::bind_cols(
      .draw = 1:length(sigma_flat),
      sigsq = sigma_flat^2
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

#' Get fitted draws from posterior of a \code{dbarts::bart}/\code{bart2} model
#'
#' @param object A \code{bart}-class model from the \code{dbarts} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included? Only applicable to continuous outcome models.
#' @param scale Should the fitted values be on the response ("probability"; for a binary/probit outcome model) or linear predictor ("linear") scale? Accepts unambiguous abbreviations (e.g. \code{"prob"}, \code{"lin"}). Default (\code{NULL}) uses the response scale for the model's outcome type. There is no separate \code{linpred_draws.bart} method - use \code{scale = "linear"} here instead, matching \code{epred_draws.wbart}/\code{.pbart}/\code{.lbart}.
#' @param ... Arguments to pass to \code{predict} (e.g. \code{dbarts:::predict.bart}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.bart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE, scale = NULL) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted_draws_dbarts(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    scale = scale
  )
}

#' Get predict draws from posterior of a \code{dbarts::bart}/\code{bart2} model
#'
#' Supports continuous outcome models (draws from \code{Normal(fitted, sigma^2)}) and binary
#' (probit) outcome models (draws from \code{Bernoulli(fitted probability)}).
#'
#' @param object A \code{bart}-class model from the \code{dbarts} package.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included? Only applicable to continuous outcome models.
#' @param ... Arguments to pass to \code{predict} (e.g. \code{dbarts:::predict.bart}); for a continuous outcome model, also accepts \code{rng} to specify the random number generator (default \code{stats::rnorm}).
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.bart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = FALSE, include_fitted = FALSE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  is_binary <- dbarts_is_binary(object)

  if (!is_binary) {
    dots <- list(...)
    rng <- if (!is.null(dots$rng)) dots$rng else stats::rnorm
    dots$rng <- NULL

    out <- do.call(fitted_draws_dbarts, c(
      list(model = object, newdata = newdata, value = ".fit", include_newdata = include_newdata, include_sigsqs = TRUE, scale = "linear"),
      dots
    ))

    out <- dplyr::mutate(out, !!value := rng(n = dplyr::n(), mean = .data$.fit, sd = sqrt(.data$sigsq)))

    if (!include_sigsqs) out <- dplyr::select(out, -"sigsq")
    if (!include_fitted) out <- dplyr::select(out, -".fit")
  } else {
    out <- fitted_draws_dbarts(
      model = object, newdata = newdata, value = ".fitted",
      ...,
      include_newdata = include_newdata,
      include_sigsqs = FALSE,
      scale = "probability"
    )

    out <- dplyr::mutate(out, !!value := stats::rbinom(dplyr::n(), 1, .data$.fitted))

    if (!include_fitted) out <- dplyr::select(out, -".fitted")
  }

  return(out)
}

#' Get residual draws for a \code{dbarts::bart}/\code{bart2} model
#'
#' Unlike \code{BART}-package models, a \code{dbarts} model fit with \code{keeptrees = TRUE}/
#' \code{keepTrees = TRUE} retains its own training response - so, unlike
#' \code{residual_draws.wbart}/\code{.pbart}, \code{response} does not need to be supplied here
#' unless the model was fit without it.
#'
#' @param object A \code{bart}-class model from the \code{dbarts} package.
#' @param newdata Data frame to generate predictions from. If omitted, original data used to fit the model.
#' @param response Original response vector. Defaults to the model's own stored response (requires
#'   \code{keeptrees = TRUE}/\code{keepTrees = TRUE}); only needs to be supplied explicitly otherwise.
#' @param value Name of the output column for residual_draws; default is \code{.residual}.
#' @param ... Arguments to pass to \code{predict} (e.g. \code{dbarts:::predict.bart}).
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included? Only applicable to continuous outcome models.
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.bart <- function(object, newdata, response = NULL, value = ".residual", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE) {
  if (missing(newdata)) newdata <- NULL
  if (!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  if (is.null(response)) {
    training_data <- object$fit$data
    if (is.null(training_data)) {
      stop("`response` was not supplied, and this model's own response can't be recovered: it was ",
           "fit with `keeptrees = FALSE`/`keepTrees = FALSE`, so `dbarts::bart`/`bart2` didn't retain ",
           "it. Either supply `response` explicitly, or refit with `keeptrees = TRUE`/`keepTrees = TRUE`.")
    }
    response <- as.vector(training_data@y)
  }

  stopifnot(is.numeric(response))

  obs <- dplyr::tibble(y = response, .row = 1:length(response))

  fitted <- epred_draws(object, newdata,
    value = ".fitted", ndraws = NULL,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    ...
  )

  out <- dplyr::mutate(
    dplyr::left_join(fitted, obs, by = ".row"),
    !!value := .data$y - .data$.fitted
  )

  dplyr::group_by(out, .row)
}

#' @export
variance_draws.bart <- function(model, value = ".sigma_sq", ...) {
  if (dbarts_is_binary(model)) {
    stop("`variance_draws()` is not applicable to a binary (probit) outcome model: its error ",
         "variance is fixed at 1, not estimated.")
  }

  sigma_draws <- combine_dbarts_sigma(model$sigma)

  dplyr::tibble(
    .chain = NA_integer_,
    .iteration = NA_integer_,
    .draw = 1:length(sigma_draws),
    !!value := sigma_draws^2
  )
}

#' @export
covariate_importance.bart <- function(model, ...) {
  vc <- model$varcount
  var_names <- if (length(dim(vc)) == 3) dimnames(vc)[[3]] else colnames(vc)

  vc <- combine_dbarts_chains(vc)
  colnames(vc) <- var_names

  vv <- colMeans(vc)

  dplyr::tibble(
    variable = names(vv),
    avg_inclusion = vv
  )
}

# stats::model.matrix()'s general contract returns a data.frame, but dbarts's
# @x slot is a plain numeric matrix - every column, including an originally
# integer-coded 0/1 treatment column, comes back as double. Restoring
# whole-valued 0/1 columns to integer matters because is_01_integer_vector()
# (used by treatment_effects()/avg_treatment_effects()/has_common_support()
# whenever no newdata/modeldata is supplied) requires exactly that class, not
# just those values.
restore_01_integer_columns <- function(data) {
  dplyr::mutate(data, dplyr::across(
    dplyr::where(~ is.double(.x) && all(.x %in% c(0, 1))),
    as.integer
  ))
}

#' @export
model.matrix.bart <- function(object, ...) {
  training_data <- object$fit$data
  if (is.null(training_data)) {
    stop("`model.matrix()` requires the model to have been fit with `keeptrees = TRUE` ",
         "(`dbarts::bart()`) / `keepTrees = TRUE` (`dbarts::bart2()`): the training data isn't ",
         "retained on the fitted object otherwise.")
  }

  restore_01_integer_columns(as.data.frame(training_data@x))
}
