#' Get treatment effect draws from posterior
#'
#' CTE = Conditional Treatment Effects (or CATE, the average effects)
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#'
#' @return A tidy data frame (tibble) with CATE values.
#' @export
#'
treatment_effects <- function(model, treatment, newdata){

  stopifnot(
    !missing(treatment),
    is.character(treatment),
    length(treatment) == 1
  )

  if(missing(newdata)){
    newdata <- stats::model.matrix(model)
  }

  stopifnot(
    treatment %in% colnames(newdata),
    is.data.frame(newdata)
  )

  stopifnot(
    is_01_integer_vector(newdata[,treatment]) | is.logical(newdata[,treatment])
  )

  treatment_class <- class( newdata[,treatment] )

  if(treatment_class == "integer"){
    treatment_on <- 1L
    treatment_off <- 0L
  } else if(treatment_class == "logical"){
    treatment_on <- TRUE
    treatment_off <- FALSE
  }

  posterior_fit_with_cf <- fitted_with_counter_factual_draws(model = model, treatment = treatment)

  posterior_treatment <- dplyr::select(
   dplyr::mutate(posterior_fit_with_cf, cte = (2 * as.integer( !!rlang::sym(treatment) ) - 1) * (observed - cfactual) ), # equivelant to treatment - non_treatment
    -observed, -cfactual)

### TODO: add removal of observations without support here if indicator argument says to use common support methodology

  return(posterior_treatment)

}

fitted_with_counter_factual_draws <- function(model, treatment){

  stopifnot(
    has_tidytreatment_methods(model)
    )

  modeldata <- stats::model.matrix(model)

  stopifnot(
    treatment %in% colnames(modeldata),
    is.data.frame(modeldata)
  )

  stopifnot(
    is_01_integer_vector(modeldata[,treatment]) | is.logical(modeldata[,treatment])
  )

  obs_fitted <- tidybayes::fitted_draws(
    model = model, value = "observed",
    newdata = modeldata,
    include_newdata = F
  )

  cfactual_fitted <- tidybayes::fitted_draws(
    model = model, value = "cfactual",
    newdata = dplyr::mutate(modeldata, !!treatment := counter_factual( !!rlang::sym(treatment) ) ),
    include_newdata = F
  )

  obs_fitted <- dplyr::left_join(
    obs_fitted,
    dplyr::mutate( dplyr::select(modeldata, !!treatment), .row = 1:dplyr::n()),
    by = c(".row")
  )

  dplyr::left_join(
    obs_fitted,
    cfactual_fitted,
    by = c(".row", ".chain", ".iteration", ".draw")
  )

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
