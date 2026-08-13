#' Get fitted draws from posterior of \code{stochtree}-package models
#'
#' @param model A model from \code{stochtree} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{epred_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real, probability scale?
#' @param ... Arguments to pass to \code{predict} (e.g. \code{stochtree:::predict.bartmodel}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
epred_draws_stochtree <- function(model, newdata = NULL, value = ".value", ..., include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  stopifnot(has_installed_package("stochtree"))

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs),
    class(model) %in% c("bartmodel"),
    ps_bart$model_params$outcome_model$link %in% c('identity','probit','cloglog') # no other links implemented
  )

  use_scale <- match.arg(scale,
    c("real", "prob"),
    several.ok = F
  )

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  if (!(missing(newdata) | is.null(newdata))) {
    posterior <- predict(model, X = newdata, terms = "y_hat", ...)
  } else {
    posterior <- extractParameter(model, term = "y_hat_train")
  }

  if (use_scale == "prob" & ps_bart$model_params$outcome_model$link == "probit") posterior <- stats::pnorm(posterior)
  if (use_scale == "prob" & ps_bart$model_params$outcome_model$link == "cloglog"){
    cloglog <- stats:::make.link("cloglog")
    posterior <- cloglog$linkinv(posterior)
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
      .draw = 1:length(model$sigma),
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
} #TEST
