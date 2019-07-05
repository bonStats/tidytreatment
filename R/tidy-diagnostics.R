#' Get residual draw for bartMachine model
#'
#' @param model bartMachine model.
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
