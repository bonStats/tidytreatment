#' Get fitted draws from posterior of \code{stan4bart}-package models
#'
#' @param model A model from \code{stan4bart} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{fitted_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real, probit or logit scale?
#' @param ... Arguments to pass to \code{predict} or \code{fitted} (e.g. \code{stan4bart:::extract}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_stan4bart <- function(model, newdata = NULL, value = ".value", ..., include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  stopifnot(has_installed_package("stan4bart"))

  if (is.null(newdata) & include_newdata) { #CHECK if we can add new data on
    stop("For models from BART package 'newdata'
          must be specified if 'include_newdata = TRUE'.")
  }

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs)
  )

  use_scale <- match.arg(scale,
                         c("real", "prob"),
                         several.ok = F
  )

  type <- switch(use_scale,
                 real = 'ev',
                 prod = 'ppd')

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  #HERE check extract type etc...

  if (!(missing(newdata) | is.null(newdata))) {
    posterior <- extract(model, newdata = newdata, type = type, ...)
  } else {
    posterior <- extract(model, type = type, ...)
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
