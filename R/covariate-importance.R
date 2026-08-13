#' Counts of variable inclusion when interacting with treatment
#'
#' @param model Model
#' @param treatment A character string specifying the name of the treatment variable.
#' @param count_once_per_tree For \code{BART}-package models: if \code{FALSE} (default), a tree
#'   in which treatment splits on multiple nodes contributes to every other variable's count once
#'   per treatment occurrence in that tree (i.e. counts scale with how often treatment itself is
#'   used to split). If \code{TRUE}, each qualifying tree contributes its variable counts exactly
#'   once, regardless of how many times treatment splits within it. Not used by all methods.
#' @param ... Arguments to pass to particular methods.
#'
#' @return Tidy data with counts of variable inclusion, when interacting with treatment variable.
#'
#' @export
covariate_with_treatment_importance <- function(model, treatment, count_once_per_tree = FALSE, ...) {
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

covariate_with_treatment_importance_BART <- function(model, treatment, count_once_per_tree = FALSE, ...) {
  # currently only use the (single) fitted BART model.
  # Whereas bartMachine uses average over replicates (default 5)
  ttree <- posterior_trees_BART(model)

  ttree_treat <- dplyr::select(
    dplyr::filter(ttree$trees, .data$var == treatment),
    "iter",
    "tree_id"
  )

  # count_once_per_tree = FALSE (default): treatment splitting on multiple
  # nodes within the same tree multiplies that tree's contribution to every
  # other variable's count (one match per treatment occurrence).
  # count_once_per_tree = TRUE: each qualifying tree contributes its node
  # counts exactly once, regardless of how many times treatment appears in it.

  if (count_once_per_tree) {
    ttree_treat <- dplyr::distinct(ttree_treat)
    join_relationship <- "one-to-many"
  } else {
    join_relationship <- "many-to-many"
  }

  # filtered to trees with treatment
  var_counts <- table(
    dplyr::left_join(ttree_treat, ttree$trees, by = c("iter", "tree_id"), relationship = join_relationship)$var,
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

#' Multinomial BART models ('mbart'/'mbart2') are not supported
#'
#' Multinomial BART models use a per-category tree representation that is incompatible with
#' this package's machinery for BART-package models, and even a working fix would not extend
#' to \code{treatment_effects()}/\code{avg_treatment_effects()}, which assume a scalar
#' continuous/binary response.
#'
#' @param model A \code{mbart} or \code{mbart2} model.
#' @param ... Not used.
#'
#' @return Does not return; always errors.
#' @export
#' @name covariate_importance-mbart-unsupported
#'
covariate_importance.mbart <- function(model, ...) {
  stop_mbart_unsupported("covariate_importance", model)
}
#' @rdname covariate_importance-mbart-unsupported
#' @export
covariate_importance.mbart2 <- function(model, ...) {
  stop_mbart_unsupported("covariate_importance", model)
}

#' @export
covariate_with_treatment_importance.wbart <- function(model, treatment, count_once_per_tree = FALSE, ...) {
  covariate_with_treatment_importance_BART(model, treatment, count_once_per_tree = count_once_per_tree, ...)
}

#' @export
covariate_with_treatment_importance.pbart <- function(model, treatment, count_once_per_tree = FALSE, ...) {
  covariate_with_treatment_importance_BART(model, treatment, count_once_per_tree = count_once_per_tree, ...)
}

#' @export
covariate_with_treatment_importance.lbart <- function(model, treatment, count_once_per_tree = FALSE, ...) {
  covariate_with_treatment_importance_BART(model, treatment, count_once_per_tree = count_once_per_tree, ...)
}

#' Multinomial BART models ('mbart'/'mbart2') are not supported
#'
#' Multinomial BART models use a per-category tree representation that is incompatible with
#' this package's machinery for BART-package models, and even a working fix would not extend
#' to \code{treatment_effects()}/\code{avg_treatment_effects()}, which assume a scalar
#' continuous/binary response.
#'
#' @param model A \code{mbart} or \code{mbart2} model.
#' @param treatment Not used.
#' @param ... Not used.
#'
#' @return Does not return; always errors.
#' @export
#' @name covariate_with_treatment_importance-mbart-unsupported
#'
covariate_with_treatment_importance.mbart2 <- function(model, treatment, ...) {
  stop_mbart_unsupported("covariate_with_treatment_importance", model)
}

#' @rdname covariate_with_treatment_importance-mbart-unsupported
#' @export
covariate_with_treatment_importance.mbart <- function(model, treatment, ...) {
  stop_mbart_unsupported("covariate_with_treatment_importance", model)
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

#' @export
covariate_importance.bartmodel <- function(model, X_train, ...) {

  stopifnot("X_train used to fit the model must be provided for stochtree package" = !missing(X_train))

  p <- length(model$train_set_metadata$feature_types)

  # granular: model$mean_forests$get_granular_split_counts(num_features = p)

  res <- dplyr::tibble(
    variable = colnames(X_train)[model$train_set_metadata$original_var_indices],
    inclusion = model$mean_forests$get_aggregate_split_counts(p)
  )

  res |>
    dplyr::group_by(.data$variable) |>
    dplyr::summarise(inclusion = sum(.data$inclusion)) |>
    dplyr::mutate(avg_inclusion = .data$inclusion / sum(.data$inclusion)) |>
    dplyr::select(-"inclusion")

}
