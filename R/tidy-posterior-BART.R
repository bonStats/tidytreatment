#' Get fitted draws from posterior of \code{BART}-package models
#'
#' @param model A model from \code{BART} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real, probit or logit scale?
#' @param ... Arguments to pass to \code{predict} (e.g. \code{BART:::predict.wbart}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
epred_draws_BART <- function(model, newdata = NULL, value = ".value", ..., include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  stopifnot(has_installed_package("BART"))

  if (is.null(newdata) & include_newdata) {
    stop("For models from BART package 'newdata'
          must be specified if 'include_newdata = TRUE'.")
  }

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    class(model) %in% c("wbart", "pbart", "lbart", "mbart", "mbart2")
  )

  use_scale <- match.arg(scale,
    c("real", "prob"),
    several.ok = F
  )

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  if (!(missing(newdata) | is.null(newdata))) {
    # S3 predict methods in BART get yhat values.
    xvars <- names(model$treedraws$cutpoints)
    bartdata <- BART::bartModelMatrix(newdata)[, xvars]
    # dodraws=TRUE => all draws (not just mean)
    posterior <- predict(object = model, newdata = bartdata, dodraws = TRUE, ...)
    if (!is.matrix(posterior)) posterior <- posterior$yhat.test
  } else {
    posterior <- model$yhat.train
  }

  if (use_scale == "prob" & "lbart" %in% class(model)) posterior <- stats::plogis(posterior)
  if (use_scale == "prob" & "pbart" %in% class(model)) posterior <- stats::pnorm(posterior)

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
    sigsq <- dplyr::bind_cols(
      .draw = 1:length(model$sigma),
      sigsq = model$sigma^2
    )

    out <- dplyr::left_join(out, sigsq, by = ".draw")

    col_order <- c(col_order, "sigsq")
  }

  # rearrange
  out <- dplyr::select(out, -!!col_order, !!col_order)

  # group
  row_groups <- names(out)[!names(out) %in% col_order[col_order != ".row"]]

  out <- dplyr::group_by(out, dplyr::across(row_groups))

  return(out)
}

#' Get predict draws from posterior of \code{BART::wbart} models
#'
#' @param object A \code{wbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param rng Random number generator function. Default is \code{rnorm} for models with Gaussian errors.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Arguments to pass to \code{predict} (e.g. \code{BART:::predict.wbart}).
#'
#' @return A tidy data frame (tibble) with predicted values.
#'
predicted_draws_BART <- function(object, newdata = NULL, value = ".prediction", ..., rng = stats::rnorm, include_newdata = TRUE, include_fitted = FALSE, include_sigsqs = FALSE) {
  stopifnot(
    is.character(value),
    is.logical(include_fitted),
    is.logical(include_sigsqs)
  )

  stopifnot(class(object) %in% "wbart")

  # get fitted values (need sigsq to start with)
  out <- epred_draws(object, newdata = newdata, value = ".fit", include_newdata = include_newdata, include_sigsqs = TRUE)

  # draw prediction from estimated variance
  out <- dplyr::mutate(out, !!value := rng(n = dplyr::n(), mean = .data$.fit, sd = sqrt(.data$sigsq)))

  # remove sigma^2 value if necessary
  if (!include_sigsqs) out <- dplyr::select(out, -.data$sigsq)

  # remove fitted value if necessary
  if (!include_fitted) out <- dplyr::select(out, -.data$.fit)

  return(out)
}


#' Get residual draw for BART model
#'
#' Classes from \code{BART}-package models
#'
#' @param object model from \code{BART} package.
#' @param response Original response vector.
#' @param newdata Data frame to generate predictions from. If omitted, original data used to fit the model.
#' @param value Name of the output column for residual_draws; default is \code{.residual}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#'
#' @return Tibble with residuals.
#'
residual_draws_BART <- function(object, response, newdata = NULL, value = ".residual", include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(response)) stop("Models from BART pacakge require response (y) as argument. Specify 'response = <y variable>' as argument.")

  stopifnot(is.numeric(response))

  obs <- dplyr::tibble(y = response, .row = 1:length(response))

  fitted <- epred_draws(object, newdata,
    value = ".fitted", ndraws = NULL,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )

  out <- dplyr::mutate(
    dplyr::left_join(fitted, obs, by = ".row"),
    !!value := .data$y - .data$.fitted
  )

  dplyr::group_by(out, .row)
}

#' Get fitted draws from posterior of \code{wbart} model
#'
#' @param object A model from \code{BART} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.wbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  epred_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )
}

#' Get fitted draws from posterior of \code{pbart} model
#'
#' @inheritParams epred_draws.wbart
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
epred_draws.pbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  epred_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )
}

#' Get fitted draws from posterior of \code{lbart} model
#'
#' @inheritParams epred_draws.wbart
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.lbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  epred_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )
}

#' Get fitted draws from posterior of \code{mbart} model
#'
#' @inheritParams epred_draws.wbart
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.mbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  epred_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )
}

#' Get fitted draws from posterior of \code{mbart2} model
#'
#' @inheritParams epred_draws.wbart
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.mbart2 <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  epred_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )
}

#' Get predict draws from posterior of \code{wbart} model
#'
#' @param object A \code{wbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Use to specify random number generator, default is \code{rng=stats::rnorm}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.wbart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = TRUE, include_fitted = FALSE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  predicted_draws_BART(
    object = object, newdata = newdata,
    value = value,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs, ...
  )
}

#' Get predict draws from posterior of \code{pbart} model
#'
#' @param object A \code{pbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param ... Use to specify random number generator, default is \code{rng=stats::rnorm}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.pbart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = TRUE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

 fitted <- epred_draws_BART(
    model = object, newdata = newdata,
    value = ".fitted",
    include_newdata = FALSE,
    include_sigsqs = FALSE,
    scale = "prob", ...
  )

 # predicted values
 dplyr::mutate(fitted, !!rlang::sym(value) := stats::rbinom(dplyr::n(), 1, .data$.fitted) )

}

#' Get predict draws from posterior of \code{lbart} model
#'
#' @param object A \code{lbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param ... Use to specify random number generator, default is \code{rng=stats::rnorm}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.lbart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = TRUE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted <- epred_draws_BART(
    model = object, newdata = newdata,
    value = ".fitted",
    include_newdata = FALSE,
    include_sigsqs = FALSE,
    scale = "prob", ...
  )

  # predicted values
  dplyr::mutate(fitted, !!rlang::sym(value) := stats::rbinom(dplyr::n(), 1, .data$.fitted) )

}

#' Get residual draw for \code{wbart} model
#'
#' The original response variable must be passed as an argument to this function.
#' e.g. `response = y`
#'
#' @param object \code{wbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, original data used to fit the model.
#' @param value Name of the output column for residual_draws; default is \code{.residual}.
#' @param ... Additional arguments passed to the underlying prediction method for the type of model given.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ndraws Not currently implemented.
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.wbart <- function(object, newdata, value = ".residual", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  residual_draws_BART(
    object = object, newdata = newdata, value = value,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs, ...
  )
}

#' Get residual draw for \code{pbart} model
#'
#' The original response variable must be passed as an argument to this function.
#' e.g. `response = y`
#'
#' @inheritParams residual_draws.wbart
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.pbart <- function(object, newdata, value = ".residual", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  residual_draws_BART(
    object = object, newdata = newdata, value = value,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs, ...
  )
}
