#' Counts of variable inclusion when interacting with treatment
#'
#' @param model Model
#' @param treatment A character string specifying the name of the treatment variable.
#' @param ... Arguments to pass to particular methods.
#'
#' @return Tidy data with counts of variable inclusion, when interacting with treatment variable.
#'
#' @export
covariate_with_treatment_importance <- function(model, treatment, ...) {
  UseMethod("covariate_with_treatment_importance")
}

#' @export
covariate_with_treatment_importance.bartMachine <- function(model, treatment, ...) {
  ii <- bartMachine::interaction_investigator(model, plot = FALSE)

  treatment_col <- colnames(ii$interaction_counts_avg) %in% treatment

  stopifnot(
    sum(treatment_col) == 1
  )

  res <- dplyr::tibble(
    variable = colnames(ii$interaction_counts_avg),
    avg_inclusion = ii$interaction_counts_avg[, treatment_col],
    sd = ii$interaction_counts_sd[, treatment_col]
  )

  dplyr::filter(res, .data$variable != treatment)
}

#' Counts of variable overall inclusion
#'
#' Inclusion metric for bartMachine and BART are scaled differently.
#' bartMachine averaged over number of trees, in addition to number of MCMC draws.
#'
#' @param model Model
#' @param ... Arguments to pass to particular methods.
#'
#' @return Tidy data with counts of variable inclusion, when interacting with treatment variable.
#' @export
#'
covariate_importance <- function(model, ...) {
  UseMethod("covariate_importance")
}

#' @export
covariate_importance.bartMachine <- function(model, ...) {
  vv <- bartMachine::get_var_props_over_chain(model, ...)

  res <- dplyr::tibble(
    variable = names(vv),
    avg_inclusion = vv
  )

  res
}

covariate_with_treatment_importance_BART <- function(model, treatment, ...) {
  # currently only use the (single) fitted BART model.
  # Whereas bartMachine uses average over replicates (default 5)
  ttree <- posterior_trees_BART(model)

  ttree_treat <- dplyr::select(
    dplyr::filter(ttree$trees, .data$var == treatment),
    .data$iter,
    .data$tree_id
  )

  # filtered to trees with treatment
  var_counts <- table(
    dplyr::left_join(ttree_treat, ttree$trees, by = c("iter", "tree_id"))$var,
    useNA = "no"
  )

  res <- dplyr::tibble(
    variable = names(var_counts),
    avg_inclusion = as.numeric(var_counts),
    sd = NA
  )

  # add vars if missing from table
  var_names <- names(model$varprob.mean)
  missing_vars <- !var_names %in% res$variable

  if (any(missing_vars)) {
    add_res <- dplyr::tibble(
      variable = var_names[missing_vars],
      avg_inclusion = 0,
      sd = NA
    )
    res <- dplyr::bind_rows(res, add_res)
  }

  dplyr::filter(res, .data$variable != treatment)
}

covariate_importance_BART <- function(model, ...) {

  # mean over mcmc draws
  vv <- model$varcount.mean

  res <- dplyr::tibble(
    variable = names(vv),
    avg_inclusion = vv
  )

  res
}

#' @export
covariate_importance.wbart <- function(model, ...) {
  covariate_importance_BART(model, ...)
}
#' @export
covariate_importance.pbart <- function(model, ...) {
  covariate_importance_BART(model, ...)
}
#' @export
covariate_importance.lbart <- function(model, ...) {
  covariate_importance_BART(model, ...)
}

#' @export
covariate_importance.mbart <- function(model, ...) {
  covariate_importance_BART(model, ...)
}
#' @export
covariate_importance.mbart2 <- function(model, ...) {
  covariate_importance_BART(model, ...)
}

#' @export
covariate_with_treatment_importance.wbart <- function(model, treatment, ...) {
  covariate_with_treatment_importance_BART(model, treatment, ...)
}

#' @export
covariate_with_treatment_importance.pbart <- function(model, treatment, ...) {
  covariate_with_treatment_importance_BART(model, treatment, ...)
}

#' @export
covariate_with_treatment_importance.lbart <- function(model, treatment, ...) {
  covariate_with_treatment_importance_BART(model, treatment, ...)
}

#' @export
covariate_with_treatment_importance.mbart2 <- function(model, treatment, ...) {
  covariate_with_treatment_importance_BART(model, treatment, ...)
}

#' @export
covariate_with_treatment_importance.mbart <- function(model, treatment, ...) {
  covariate_with_treatment_importance_BART(model, treatment, ...)
}

#' @export
covariate_importance.stan4bartFit <- function(model, ...) {

  # extract mcmc draws
  vv <- dbarts::extract(model, type = "varcount", combine_chains = F, include_warmup = F)

  res <- dplyr::tibble(
    variable = dimnames(vv)$predictor,
    avg_inclusion = rowMeans(vv)
  )

  res
}

#' @export
covariate_importance.bartcFit <- function(model, fitstage = c("response","assignment"), ...) {

  fitstage <- match.arg(fitstage)

  if(fitstage == "response"){
    covariate_importance(model$fit.rsp, ...)
  } else {
    covariate_importance(model$fit.trt, ...)
  }

}
