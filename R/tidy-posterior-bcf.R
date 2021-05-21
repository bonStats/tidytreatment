#' Get fitted draws from posterior of \code{bcf}-package models
#'
#' @param model A model from \code{bcf} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{fitted_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_bcf <- function(model, newdata = NULL, value = ".value", include_newdata = T, include_sigsqs = F){

  stopifnot(has_installed_package("bcf"))

  if( is.null(newdata) & include_newdata)
    stop("For models from bcf package 'newdata'
         must be specified if 'include_newdata = T'.
         Use original data used to fit the model.")

  if(!is.null(newdata)) warning("'newdata' is data used to fit the model for bcf models.
                                Supplying alternative data to predict from is not possible")

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    class(model) %in% c("bcf")
  )

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  posterior <- model$yhat

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if(include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(t(posterior), .name_repair = function(names){ paste0(".col_iter", as.character(1:length(names)) ) }),
    .row = 1:ncol(posterior)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = !!value, dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer( gsub(pattern = ".col_iter", replacement = "", x =.draw) ) )

  # include sigma^2 if needed
  if(include_sigsqs){

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
  row_groups <- names(out)[ ! names(out) %in% col_order[col_order != ".row"] ]

  out <- dplyr::group_by(out, dplyr::across(row_groups))

  return(out)

}

#' Get fitted draws from posterior of \code{bcf2} model
#'
#' @param model A model fit with \code{tidytreatment::bcf2} function.
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
fitted_draws.bcf <- function(model, newdata, value = ".value", ..., n = NULL, include_newdata = T, include_sigsqs = F){

  if(missing(newdata)){
    newdata <- NULL
  }

  fitted_draws_bcf(model = model, newdata = newdata, value = value,
                   ...,
                   include_newdata = include_newdata,
                   include_sigsqs = include_sigsqs)

}

