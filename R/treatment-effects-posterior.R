#' Get (individual) treatment effect draws from posterior
#'
#' CTE = Conditional Treatment Effects (usually used to generate (C)ATE or ATT)
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

treatment_effects <- function(model, treatment, newdata, subset = "all", common_support_method, cutoff, ...){

  UseMethod("treatment_effects")

}

#' Get treatment effect draws from posterior
#'
#' CTE = Conditional Treatment Effects (or CATE, the average effects)
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#' Assumes treated column is either a integer column of 1's (treated) and 0's (nontreated) or logical indicating treatment if TRUE.
#'
#' @inheritParams treatment_effects
#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
treatment_effects.default <- function(model, treatment, newdata, subset = "all", common_support_method, cutoff, ...){

  stopifnot(
    !missing(treatment),
    is.character(treatment),
    length(treatment) == 1
  )

  if(missing(newdata)){

    check_method(model, method = "model.matrix",
                 helper = "Please specify 'newdata' argument = data from model fitting.")

    modeldata <- stats::model.matrix(model)

  } else {

    modeldata <- newdata

  }

  posterior_fit_with_cf <- fitted_with_counter_factual_draws(model, modeldata, treatment, subset, ...)

  posterior_treatment <- dplyr::select(
   dplyr::mutate(posterior_fit_with_cf, cte = (2L * as.integer( !!rlang::sym(treatment) ) - 1L) * (observed - cfactual) ), # equivalent to treatment - non_treatment
    -observed, -cfactual)

  # add boolean for common support
  if(!missing(common_support_method)){

    stopifnot(
      !missing(cutoff),
      # should use model data only, unless needs to be specified (e.g. for BART models).
      missing(newdata) | !has_tidytreatment_methods(model)
    )

    if( !missing(newdata) ) message("Note: Argument 'newdata' must be original dataset when calculating common support.")

    common_supp <-
      calc_common_support_from_fitted_and_cf(
        fitted_and_cf = posterior_fit_with_cf,
        modeldata = modeldata,
        treatment = treatment,
        method = common_support_method,
        cutoff = cutoff
      )

    posterior_treatment <- dplyr::left_join(posterior_treatment, common_supp, by = ".row")

  }


  return(posterior_treatment)

}

fitted_with_counter_factual_draws <- function(model, newdata, treatment, subset, ...){

  stopifnot(
    has_tidytreatment_methods(model) | !missing(newdata)
    )

  if(missing(newdata)){
    newdata <- stats::model.matrix(model)
  }

  use_subset <- match.arg( subset, c("all","treated","nontreated") )

 stopifnot(
    treatment %in% colnames(newdata),
    is.data.frame(newdata)
  )

  stopifnot(
    is_01_integer_vector(newdata[,treatment]) | is.logical(newdata[,treatment])
  )

  obs_fitted <- tidybayes::fitted_draws(
    model = model, value = "observed",
    newdata = newdata,
    include_newdata = F,
    ...
  )

  cfactual_fitted <- tidybayes::fitted_draws(
    model = model, value = "cfactual",
    newdata = dplyr::mutate(newdata, !!treatment := counter_factual( !!rlang::sym(treatment) ) ),
    include_newdata = F,
    ...
  )

  obs_fitted <- dplyr::left_join(
    obs_fitted,
    dplyr::mutate( dplyr::select(newdata, !!treatment), .row = 1:dplyr::n()),
    by = c(".row")
  )

  out <- dplyr::left_join(
    obs_fitted,
    cfactual_fitted,
    by = c(".row", ".chain", ".iteration", ".draw")
  )

  if(use_subset == "treated"){

    out <- dplyr::filter(out, is_treated( !!rlang::sym(treatment) ) )

  } else if(use_subset == "nontreated"){

    out <- dplyr::filter(out, !is_treated( !!rlang::sym(treatment) ) )

  }

  return(out)

}

counter_factual <- function(x){

  if( is.integer(x) ){

    return( 1L - x )

  } else if( is.logical(x) ) {

    return( !x )

  } else {

    return( rep(NA, times = length(x)) )

  }

}

is_treated <- function(x){

  if( is.integer(x) ){

    return( x == 1L )

  } else if( is.logical(x) ) {

    return( x )

  } else {

    return( rep(NA, times = length(x)) )

  }

}

#' Get treatment effect draws from posterior
#'
#' CTE = Conditional Treatment Effects (or CATE, the average effects)
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#' Assumes treated column is either a integer column of 1's (treated) and 0's (nontreated) or logical indicating treatment if TRUE.
#'
#' @inheritParams treatment_effects
#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
treatment_effects.bcf <- function(model, treatment, newdata, subset = "all", common_support_method, cutoff){

  stopifnot(
    !missing(treatment),
    is.character(treatment),
    length(treatment) == 1
  )

  if(!missing(common_support_method)) stop("Common support not implemented for bcf models.")
  if(!missing(newdata)){
    warning("Only use original fitted data with bcf models.")
    include_newdata <- T
  } else {
    include_newdata <- F
  }

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", "cte")

  posterior <- model$treatment_effect

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if(include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(t(posterior), .name_repair = function(names){ paste0(".col_iter", as.character(1:length(names)) ) }),
    .row = 1:ncol(posterior)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = 'cte', dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer( gsub(pattern = ".col_iter", replacement = "", x =.draw) ) )

  # rearrange
  out <- dplyr::select(out, -!!col_order, !!col_order)

  # group
  row_groups <- names(out)[ ! names(out) %in% col_order[col_order != ".row"] ]

  out <- dplyr::group_by(out, dplyr::across(row_groups))


  return(out)

}
