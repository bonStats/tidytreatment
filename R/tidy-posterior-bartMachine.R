#' Get fitted draws from posterior of \code{bartMachine} model
#'
#' @param object A \code{bartMachine} model.
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
epred_draws.bartMachine <- function(object, newdata, value = ".value", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  if (missing(newdata)) newdata <- stats::model.matrix(object)

  stopifnot(
    is.data.frame(newdata),
    is.character(value),
    is.null(ndraws) | (is.integer(ndraws) & ndraws > 0),
    is.logical(include_newdata),
    is.logical(include_sigsqs)
  )

  if(!is.null(ndraws)) warning("Argument `ndraws` ignored: not implemented")

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  posterior <- bartMachine::bart_machine_get_posterior(bart_machine = object, new_data = newdata)

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if (include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(posterior$y_hat_posterior_samples, .name_repair = function(names) {
      paste0(".col_iter", as.character(1:length(names)))
    }),
    .row = 1:nrow(newdata)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = !!value, dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer(gsub(pattern = ".col_iter", replacement = "", x = .data$.draw)))

  # include sigma^2 if needed
  if (include_sigsqs) {
    sigsq <- dplyr::bind_cols(
      .draw = 1:object$num_iterations_after_burn_in,
      sigsq = bartMachine::get_sigsqs(object)
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


#' Get predict draws from posterior of \code{bartMachine} model
#'
#' @param object A \code{bartMachine} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param value The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ndraws Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.bartMachine <- function(object, newdata, value = ".prediction", ..., ndraws = NULL, include_newdata = TRUE, include_fitted = FALSE, include_sigsqs = FALSE) {
  stopifnot(
    is.character(value),
    is.logical(include_fitted),
    is.logical(include_sigsqs)
  )

  # get fitted values (need sigsq to start with)
  out <- epred_draws.bartMachine(object = object, newdata = newdata, value = ".fit", include_newdata = include_newdata, include_sigsqs = TRUE)

  # draw prediction from estimated variance
  out <- dplyr::mutate(out, !!value := stats::rnorm(n = dplyr::n(), mean = .data$.fit, sd = sqrt(.data$sigsq)))

  # remove sigma^2 value if necessary
  if (!include_sigsqs) out <- dplyr::select(out, -.data$sigsq)

  # remove fitted value if necessary
  if (!include_fitted) out <- dplyr::select(out, -.data$.fit)

  return(out)
}

#' Get residual draw for \code{bartMachine} model
#'
#' @param object \code{bartMachine} model.
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
residual_draws.bartMachine <- function(object, newdata, value = ".residual", ..., ndraws = NULL, include_newdata = TRUE, include_sigsqs = FALSE) {
  obs <- dplyr::tibble(y = object$y, .row = 1:object$n)

  fitted <- epred_draws(object, newdata,
    value = ".fitted", ndraws = ndraws,
    include_newdata = include_newdata,
    include_sigsqs = include_sigsqs
  )


  out <- dplyr::mutate(
    dplyr::left_join(fitted, obs, by = ".row"),
    !!value := .data$y - .data$.fitted
  )

  dplyr::group_by(out, .data$.row)
}
