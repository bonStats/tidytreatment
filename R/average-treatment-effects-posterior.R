#' Get (conditional) average treatment effect draws from posterior
#'
#' (C)ATE = (Conditional) Average Treatment Effects
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#' Assumes treated column is either a integer column of 1's (treated) and 0's (nontreated) or logical indicating treatment if TRUE.
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param subset Either "treated", "nontreated", or "all". Default is "all".
#' @param common_support_method Either "sd", or "chisq". Default is unspecified, and no common support calculation is done.
#' @param cutoff Cutoff for common support (if in use).
#' @param ... Arguments to be passed to \code{tidybayes::fitted_draws} typically scale for \code{BART} models.
#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
#'
avg_treatment_effects <- function(model, treatment, newdata, subset = "all", common_support_method, cutoff, ...) {
  te <- dplyr::group_by(
    .data = treatment_effects(
      model = model, treatment = treatment,
      newdata = newdata, subset = subset,
      common_support_method = common_support_method,
      cutoff = cutoff, ...
    ),
    .data$.chain, .data$.iteration, .data$.draw
  )

  dplyr::summarise(te, ate = mean(.data$cte), .groups = "drop")
}

#' Get average treatment effect draws from posterior
#'
#' ATE = Average Treatment Effects
#' Assumes treated column is either a integer column of 1's (treated) and 0's (nontreated) or logical indicating treatment if TRUE.
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param common_support_method Either "sd", or "chisq". Default is unspecified, and no common support calculation is done.
#' @param cutoff Cutoff for common support (if in use).
#' @param ... Arguments to be passed to \code{tidybayes::fitted_draws} typically scale for \code{BART} models.
#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
#'
tidy_ate <- function(model, treatment, common_support_method, cutoff, ...) {
  .dots <- list(...)
  if (!"newdata" %in% names(.dots)) check_method(model, method = "model.matrix", helper = "Please use 'avg_treatment_effects' function with 'newdata'.")

  te <- dplyr::group_by(
    .data = treatment_effects(
      model = model, treatment = treatment,
      subset = "all",
      common_support_method = common_support_method,
      cutoff = cutoff, ...
    ),
    .data$.chain, .data$.iteration, .data$.draw
  )

  dplyr::summarise(te, ate = mean(.data$cte), .groups = "drop")
}

#' Get average treatment effect on treated draws from posterior
#'
#' ATT = average Treatment Effects on Treated
#' Assumes treated column is either a integer column of 1's (treated) and 0's (nontreated) or logical indicating treatment if TRUE.
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param common_support_method Either "sd", or "chisq". Default is unspecified, and no common support calculation is done.
#' @param cutoff Cutoff for common support (if in use).
#' @param ... Arguments to be passed to \code{tidybayes::fitted_draws} typically scale for \code{BART} models.
#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
#'
tidy_att <- function(model, treatment, common_support_method, cutoff, ...) {
  .dots <- list(...)
  if (!"newdata" %in% names(.dots)) check_method(model, method = "model.matrix", helper = "Please use 'avg_treatment_effects' function with 'newdata'.")

  te <- dplyr::group_by(
    .data = treatment_effects(
      model = model, treatment = treatment,
      subset = "treated",
      common_support_method = common_support_method,
      cutoff = cutoff, ...
    ),
    .data$.chain, .data$.iteration, .data$.draw
  )

  dplyr::summarise(te, att = mean(.data$cte), .groups = "drop")
}
