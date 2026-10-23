# wbart: model$sigma is a plain vector for one chain (or mc.wbart(mc.cores = 1)),
# a [samples x mc.cores] matrix (one column per chain) when mc.wbart() combined
# several - yhat.train is rbind()'d in the same chain-major block order.
# pbart/lbart have no equivalent field: mc.pbart()/mc.lbart() only rbind()
# when combining, so single- vs multi-chain can't be told apart; .chain/
# .iteration stay NA for those.
bart_chain_iteration_index <- function(model, n_total) {
  if (!identical(class(model)[1], "wbart")) {
    return(list(chain = rep(NA_integer_, n_total), iteration = rep(NA_integer_, n_total)))
  }

  if (!is.matrix(model$sigma)) {
    return(list(chain = rep(1L, n_total), iteration = seq_len(n_total)))
  }

  n_chains <- ncol(model$sigma)
  n_per_chain <- n_total / n_chains

  stopifnot(
    "wbart's yhat.train row count isn't a whole multiple of sigma's chain count - can't recover .chain/.iteration" =
      n_per_chain == round(n_per_chain)
  )

  list(chain = rep(seq_len(n_chains), each = n_per_chain), iteration = rep(seq_len(n_per_chain), times = n_chains))
}

# model$sigma prepends nskip burn-in draws to the ndpost kept draws
# (cwbart.cpp writes sdraw[i] every iteration, yhat.train only once burn-in
# ends). The correct post-burn-in entries are the *last* n_total, exact when
# keepevery = 1 (BART's default; keepevery isn't retained on the model, so
# otherwise this is close but not guaranteed exact). Multi-chain sigma: tail
# taken per-chain (per column) before flattening.
bart_sigma_aligned <- function(model, n_total) {
  sigma <- model$sigma

  if (!is.matrix(sigma)) {
    return(utils::tail(sigma, n_total))
  }

  n_chains <- ncol(sigma)
  n_per_chain <- n_total / n_chains
  as.vector(utils::tail(sigma, n_per_chain))
}

#' Get fitted draws from posterior of \code{BART}-package models
#'
#' @param model A model from \code{BART} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale What scale should the returned values be on? Default uses the response scale for the model's outcome type. Other options: \code{scale="probability"} (applicable only for binary/probit outcome model), and \code{scale="linear"} for linear predictor.
#' @param ... Arguments to pass to \code{predict} (e.g. \code{BART:::predict.wbart}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_BART <- function(model, newdata = NULL, value = ".value", ..., include_newdata = FALSE, include_sigsqs = FALSE, scale) {
  stopifnot(has_installed_package("BART"))

  if (is.null(newdata) & include_newdata) {
    stop("`newdata` was not supplied, but `include_newdata = TRUE`: `wbart`/`pbart`/`lbart` models ",
         "from the BART package don't store the data they were fitted on, so there's nothing to ",
         "attach to the output. Either supply `newdata` explicitly, or set `include_newdata = FALSE` ",
         "if you don't need the fitted data attached.")
  }

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    class(model) %in% c("wbart", "pbart", "lbart")
  )

  use_scale <- match.arg(scale,
    c("linear", "probability"),
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

  if (use_scale == "probability" & "lbart" %in% class(model)) posterior <- stats::plogis(posterior)
  if (use_scale == "probability" & "pbart" %in% class(model)) posterior <- stats::pnorm(posterior)

  # joined onto `out` below by .draw, once .draw is recovered as an integer
  chain_index <- bart_chain_iteration_index(model, nrow(posterior))
  chain_lookup <- dplyr::tibble(.draw = seq_len(nrow(posterior)), .chain = chain_index$chain, .iteration = chain_index$iteration)

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

  # recover .draw as an integer, then attach the real .chain/.iteration each draw came from
  out <- dplyr::mutate(out, .draw = as.integer(gsub(pattern = ".col_iter", replacement = "", x = .data$.draw)))
  out <- dplyr::left_join(out, chain_lookup, by = ".draw")

  # include sigma^2 if needed
  if (include_sigsqs) {
    sigma_aligned <- bart_sigma_aligned(model, nrow(posterior))
    sigsq <- dplyr::bind_cols(
      .draw = seq_along(sigma_aligned),
      sigsq = sigma_aligned^2
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

#' Get predict draws from posterior of \code{BART::wbart} models
#'
#' @param object A \code{wbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param rng Random number generator function. Default is \code{rnorm} for models with Gaussian errors.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Arguments to pass to \code{predict} (e.g. \code{BART:::predict.wbart}).
#'
#' @return A tidy data frame (tibble) with predicted values.
#'
predicted_draws_BART <- function(object, newdata = NULL, value = ".prediction", ..., rng = stats::rnorm, include_newdata = FALSE, include_fitted = FALSE, include_sigsqs = FALSE) {
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
  if (!include_sigsqs) out <- dplyr::select(out, -"sigsq")

  # remove fitted value if necessary
  if (!include_fitted) out <- dplyr::select(out, -".fit")

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
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#'
#' @return Tibble with residuals.
#'
residual_draws_BART <- function(object, response, newdata = NULL, value = ".residual", include_newdata = FALSE, include_sigsqs = FALSE) {
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
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale What scale should the returned values be on? Default uses the response scale for the model's outcome type. Other options: \code{scale="probability"} (applicable only for binary/probit outcome model), and \code{scale="linear"} for linear predictor. Has no effect for \code{wbart}, which has no link function.
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.wbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE, scale = "linear") {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    scale = scale
  )
}

#' Get fitted draws from posterior of \code{pbart} model
#'
#' @inheritParams epred_draws.wbart
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
epred_draws.pbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE, scale = "probability") {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    scale = scale
  )
}

#' Get fitted draws from posterior of \code{lbart} model
#'
#' @inheritParams epred_draws.wbart
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
epred_draws.lbart <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE, scale = "probability") {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted_draws_BART(
    model = object, newdata = newdata, value = value,
    ...,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs,
    scale = scale
  )
}

#' Multinomial BART models ('mbart'/'mbart2') are not supported
#'
#' Multinomial BART models use a per-category tree representation that is incompatible with
#' this package's machinery for BART-package models. A working fix would not extend
#' to \code{treatment_effects()}/\code{avg_treatment_effects()}, which assume a scalar
#' continuous/binary response.
#'
#' @param object A \code{mbart} or \code{mbart2} model.
#' @param ... Not used.
#'
#' @return Always returns error.
#' @export
#' @name mbart-unsupported
#'
epred_draws.mbart <- function(object, ...) {
  stop_mbart_unsupported("epred_draws", object)
}

#' @rdname mbart-unsupported
#' @export
epred_draws.mbart2 <- function(object, ...) {
  stop_mbart_unsupported("epred_draws", object)
}

#' @rdname mbart-unsupported
#' @export
predicted_draws.mbart <- function(object, ...) {
  stop_mbart_unsupported("predicted_draws", object)
}

#' @rdname mbart-unsupported
#' @export
predicted_draws.mbart2 <- function(object, ...) {
  stop_mbart_unsupported("predicted_draws", object)
}

#' @rdname mbart-unsupported
#' @export
residual_draws.mbart <- function(object, ...) {
  stop_mbart_unsupported("residual_draws", object)
}

#' @rdname mbart-unsupported
#' @export
residual_draws.mbart2 <- function(object, ...) {
  stop_mbart_unsupported("residual_draws", object)
}

#' Get predict draws from posterior of \code{wbart} model
#'
#' @param object A \code{wbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Use to specify random number generator, default is \code{rng=stats::rnorm}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.wbart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = FALSE, include_fitted = FALSE, include_sigsqs = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  predicted_draws_BART(
    object = object, newdata = newdata,
    value = value,
    include_newdata = include_newdata,
    include_fitted = include_fitted,
    include_sigsqs = include_sigsqs, ...
  )
}

#' Get predict draws from posterior of \code{pbart} model
#'
#' @param object A \code{pbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_fitted Should the posterior fitted values be included in the tibble? Default \code{FALSE}.
#' @param ... Use to specify random number generator, default is \code{rng=stats::rnorm}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.pbart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = FALSE, include_fitted = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

 fitted <- fitted_draws_BART(
    model = object, newdata = newdata,
    value = ".fitted",
    include_newdata = include_newdata,
    include_sigsqs = FALSE,
    scale = "probability", ...
  )

 # predicted values
 out <- dplyr::mutate(fitted, !!rlang::sym(value) := stats::rbinom(dplyr::n(), 1, .data$.fitted) )

 if (!include_fitted) out <- dplyr::select(out, -".fitted")

 out
}

#' Get predict draws from posterior of \code{lbart} model
#'
#' @param object A \code{lbart} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_fitted Should the posterior fitted values be included in the tibble? Default \code{FALSE}.
#' @param ... Use to specify random number generator, default is \code{rng=stats::rnorm}.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.lbart <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = FALSE, include_fitted = FALSE) {
  if (missing(newdata)) {
    newdata <- NULL
  }

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  fitted <- fitted_draws_BART(
    model = object, newdata = newdata,
    value = ".fitted",
    include_newdata = include_newdata,
    include_sigsqs = FALSE,
    scale = "probability", ...
  )

  # predicted values
  out <- dplyr::mutate(fitted, !!rlang::sym(value) := stats::rbinom(dplyr::n(), 1, .data$.fitted) )

  if (!include_fitted) out <- dplyr::select(out, -".fitted")

  out

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
#' @param include_newdata Should the newdata be included in the tibble? Default \code{FALSE}.
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ndraws Not currently implemented.
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.wbart <- function(object, newdata, value = ".residual", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE) {
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
residual_draws.pbart <- function(object, newdata, value = ".residual", ..., ndraws = NULL, include_newdata = FALSE, include_sigsqs = FALSE) {
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

#' Tidy access to posterior of a \code{wbart} model
#'
#' Returns \code{sigma} (the residual standard deviation actually sampled by the model - see
#' \code{\link{variance_draws}} for \code{sigma^2}) alongside \code{.chain}/\code{.iteration}/
#' \code{.draw}, aligned and chain-labelled the same way \code{epred_draws.wbart()} is (see
#' \code{bart_chain_iteration_index()}/\code{bart_sigma_aligned()} in the package source).
#'
#' @param model A \code{wbart} model from the \code{BART} package.
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) of posterior draws.
#' @export
#'
tidy_draws.wbart <- function(model, ...) {
  n_total <- nrow(model$yhat.train)
  chain_index <- bart_chain_iteration_index(model, n_total)

  dplyr::tibble(
    .chain = chain_index$chain,
    .iteration = chain_index$iteration,
    .draw = seq_len(n_total),
    sigma = bart_sigma_aligned(model, n_total)
  )
}

#' Tidy access to posterior of a \code{pbart}/\code{lbart} model
#'
#' @param model A \code{pbart} or \code{lbart} model from the \code{BART} package.
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) of posterior draws.
#' @export
#' @name tidy_draws-pbart-lbart
#'
tidy_draws.pbart <- function(model, ...) {
  n_total <- nrow(model$yhat.train)
  chain_index <- bart_chain_iteration_index(model, n_total)

  warning(
    "BART::pbart has no per-draw non-tree scalar parameters. This function returns an (essentially) empty tibble."
  )

  dplyr::tibble(
    .chain = chain_index$chain,
    .iteration = chain_index$iteration,
    .draw = seq_len(n_total)
  )
}

#' @rdname tidy_draws-pbart-lbart
#' @export
tidy_draws.lbart <- function(model, ...) {
  n_total <- nrow(model$yhat.train)
  chain_index <- bart_chain_iteration_index(model, n_total)

  warning(
    "BART::lbart has no per-draw non-tree scalar parameters. This function returns an (essentially) empty tibble."
  )

  dplyr::tibble(
    .chain = chain_index$chain,
    .iteration = chain_index$iteration,
    .draw = seq_len(n_total)
  )
}

#' Multinomial BART models ('mbart'/'mbart2') are not supported
#'
#' Multinomial BART models use a per-category tree representation that is incompatible with
#' this package's machinery for BART-package models.
#'
#' @param model A \code{mbart} or \code{mbart2} model.
#' @param ... Not used.
#'
#' @return Does not return; always errors.
#' @export
#' @name tidy_draws-mbart-unsupported
#'
tidy_draws.mbart <- function(model, ...) {
  stop_mbart_unsupported("tidy_draws", model)
}

#' @rdname tidy_draws-mbart-unsupported
#' @export
tidy_draws.mbart2 <- function(model, ...) {
  stop_mbart_unsupported("tidy_draws", model)
}
