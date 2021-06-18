#' Counts of variable inclusion when interacting with treatment
#'
#' @param model Model
#' @param treatment A character string specifying the name of the treatment variable.
#' @param ... Arguments to pass to particular methods.
#'
#' @return Tidy data with counts of variable inclusion, when interacting with treatment variable.
#' @export
#'
covariate_with_treatment_importance <- function(model, treatment, ...){

  UseMethod("covariate_with_treatment_importance")

}

#' @export
covariate_with_treatment_importance.bartMachine  <- function(model, treatment, ...){

  ii <- bartMachine::interaction_investigator(model, plot = FALSE)

  treatment_col <-  colnames(ii$interaction_counts_avg) %in% treatment

  stopifnot(
     sum(treatment_col) == 1
    )

  res <- dplyr::tibble(
    variable = colnames(ii$interaction_counts_avg),
    avg_inclusion = ii$interaction_counts_avg[,treatment_col],
    sd = ii$interaction_counts_sd[,treatment_col]
  )

  dplyr::filter(res, .data$variable != treatment)

}

#' Counts of variable overall inclusion
#'
#' @param model Model
#' @param ... Arguments to pass to particular methods.
#'
#' @return Tidy data with counts of variable inclusion, when interacting with treatment variable.
#' @export
#'
covariate_importance <- function(model, ...){

  UseMethod("covariate_importance")

}

#' @export
covariate_importance.bartMachine  <- function(model, ...){

  vv <- bartMachine::get_var_props_over_chain(model, ...)

  res <- dplyr::tibble(
    variable = names(vv),
    avg_inclusion = vv
  )

  res

}
