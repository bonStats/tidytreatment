#' Get fitted draws from posterior of \code{bartMachine} model
#'
#' @param model A \code{bartMachine} model.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{fitted_draws}; default \code{".value"}.
#' @param n Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) with fitted values.
#' @export
#'
fitted_draws.bartMachine <- function(model, newdata, value = ".value", ..., n = NULL, include_newdata = T, include_sigsqs = F){

  if(missing(newdata)) newdata <- stats::model.matrix(model)

  stopifnot(
    is.data.frame(newdata),
    is.character(value),
    is.null(n) | (is.integer(n) & n > 0),
    is.logical(include_newdata),
    is.logical(include_sigsqs)
    )

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  posterior <- bartMachine::bart_machine_get_posterior(bart_machine = model, new_data = newdata)

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if(include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(posterior$y_hat_posterior_samples, .name_repair = function(names){ paste0(".col_iter", as.character(1:length(names)) ) }),
    .row = 1:nrow(newdata)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = !!value, dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer( gsub(pattern = ".col_iter", replacement = "", x =.draw) ) )

  # include sigma^2 if needed
  if(include_sigsqs){

    sigsq <- dplyr::bind_cols(
      .draw = 1:model$num_iterations_after_burn_in,
      sigsq = bartMachine::get_sigsqs(model)
    )

    out <- dplyr::left_join(out, sigsq, by = ".draw")

    col_order <- c(col_order, "sigsq")

  }

  # rearrange
  out <- dplyr::select(out, -!!col_order, !!col_order)

  # group
  row_groups <- names(out)[ ! names(out) %in% col_order[col_order != ".row"] ]

  out <- dplyr::group_by_(out, .dots = row_groups)

  return(out)

}


#' Get predict draws from posterior of \code{bartMachine} model
#'
#' @param model A \code{bartMachine} model.
#' @param newdata Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.
#' @param prediction The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param n Not currently implemented.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_fitted Should the posterior fitted values be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param ... Not currently in use.
#'
#' @return A tidy data frame (tibble) with predicted values.
#' @export
#'
predicted_draws.bartMachine <- function(model, newdata, prediction = ".prediction", ..., n = NULL, include_newdata = T, include_fitted = F, include_sigsqs = F){

  stopifnot(
    is.character(prediction),
    is.logical(include_fitted),
    is.logical(include_sigsqs)
  )

  # get fitted values (need sigsq to start with)
  out <- fitted_draws.bartMachine(model = model, newdata = newdata, value = ".fit", include_newdata = include_newdata, include_sigsqs = T)

  # draw prediction from estimated variance
  out <- dplyr::mutate(out, !!prediction := stats::rnorm(n = n(), mean = .fit, sd = sqrt(sigsq) ) )

  # remove sigma^2 value if necessary
  if(!include_sigsqs){

    out <- dplyr::select(out, -sigsq)

  }

  # remove fitted value if necessary
  if(!include_fitted){

    out <- dplyr::select(out, -.fit)

  }

  return(out)

}

#' Get residual draw for \code{bartMachine} model
#'
#' @param model \code{bartMachine} model.
#' @param newdata Data frame to generate predictions from. If omitted, original data used to fit the model.
#' @param residual Name of the output column for residual_draws; default is \code{.residual}.
#' @param ... Additional arguments passed to the underlying prediction method for the type of model given.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param n Not currently implemented.
#'
#' @return Tibble with residuals.
#' @export
#'
residual_draws.bartMachine <- function(model, newdata, residual = ".residual", ..., n = NULL, include_newdata = T, include_sigsqs = F){

  obs <- tibble::tibble(y = model$y, .row = 1:model$n)

  fitted <- fitted_draws(model, newdata, value = ".fitted", n = NULL,
                         include_newdata = include_newdata,
                         include_sigsqs = include_sigsqs)


  out <- dplyr::mutate(
    dplyr::left_join(fitted, obs, by = ".row"),
    !!residual := y - .fitted)

  group_by(out, .row)

}


