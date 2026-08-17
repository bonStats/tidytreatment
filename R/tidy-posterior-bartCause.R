#' Get expected prediction draws from posterior of \code{bartCause}-package objects
#'
#' Typically referred to as fitted value draws on response scale, where appropriate.
#'
#' @param object A \code{bartCauseFit} object.
#' @param ... Additional arguments (e.g. \code{newdata}) passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#' @param fitstage If \code{is.null(type)}, return posterior from \code{response} or treatment \code{assignment} model.
#'
#' @export
epred_draws.bartcFit = function(
    object, ...,
    value = ".epred", fitstage = c("response","assignment")
) {

  fitstage <- match.arg(fitstage)

  if(fitstage == "response"){
    draws = tidybayes::epred_draws(object$fit.rsp, ..., value = value)
  } else {
    draws = tidybayes::epred_draws(object$fit.trt, ..., value = value)
  }

  return(draws)
}

#' Get prediction draws from posterior of \code{bartCause}-package objects
#'
#' @param object A \code{bartCauseFit} object.
#' @param ... Additional arguments (e.g. \code{newdata}) passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#' @param fitstage If \code{is.null(type)}, return posterior from \code{response} or treatment \code{assignment} model.
#'
#' @export
predicted_draws.bartcFit = function(
    object, ...,
    value = ".prediction", fitstage = c("response","assignment")
) {

  fitstage <- match.arg(fitstage)

  if(fitstage == "response"){
    draws = tidybayes::predicted_draws(object$fit.rsp, ..., value = value)
  } else {
    draws = tidybayes::predicted_draws(object$fit.trt, ..., value = value)
  }

  return(draws)
}

#' Get expected prediction draws (on linear scale) from posterior of \code{bartCause}-package objects
#'
#' Typically referred to as fitted value draws on linear scale, where appropriate.
#'
#' @param object A \code{bartCauseFit} object.
#' @param ... Additional arguments (e.g. \code{newdata}) passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#' @param fitstage If \code{is.null(type)}, return posterior from \code{response} or treatment \code{assignment} model.
#'
#' @export
linpred_draws.bartcFit = function(
    object, ...,
    value = ".linpred", fitstage = c("response","assignment")
) {

  fitstage <- match.arg(fitstage)

  if(fitstage == "response"){
    draws <- tidybayes::linpred_draws(object$fit.rsp, ..., value = value)
  } else {
    draws <- tidybayes::linpred_draws(object$fit.trt, ..., value = value)
  }

  return(draws)
}

utils::globalVariables(c("var1"))

# Types documented in ?bartCause::extract that return one value per
# observation, as opposed to a single value per posterior draw
# (pate/sate/cate/sigma). Used by tidy_draws.bartcFit() below.
bartcause_per_unit_types <- c("mu.obs", "mu.cf", "mu.0", "mu.1", "y.cf", "y.0", "y.1", "icate", "ite", "p.score", "p.weights")

#' Tidy access to posterior of \code{bartCause}-package objects
#'
#' @param model A \code{bartCauseFit} object.
#' @param type Posterior quantity to return. See \code{\link[bartCause]{bartc-generics}}.
#' @param fitstage If \code{is.null(type)}, return posterior from \code{response} or treatment \code{assignment} model.
#' @param ... Additional parameters passed up the generic method chain.
#'
#' @export
tidy_draws.bartcFit = function(model, type = NULL, fitstage = c("response","assignment"), ...) {

  if(is.null(type)){
    fitstage <- match.arg(fitstage)

    if(fitstage == "response"){
      draws <- tidybayes::tidy_draws(model$fit.rsp, ...)
    } else {
      draws <- tidybayes::tidy_draws(model$fit.trt, ...)
    }
    return(draws)
  }

  # type != NULL
  if(length(fitstage) > 1) warning("fitstage ignored when type != NULL")

  ldots <- list(...)

  if(is.null(ldots$newdata)){
    sample_array <- dbarts::extract(model, type = type, ..., combineChains = FALSE)
  } else {
    sample_array <- predict(model, newdata = ldots$newdata, type = type, ..., combineChains = FALSE)
  }

  ndim <- length(dim(sample_array))
  # dbarts::extract(..., combineChains = FALSE) drops the chain dimension
  # entirely when the model was fit with a single chain (n.chains = 1), so a
  # 2D array is ambiguous: for a per-unit `type` (one value per observation,
  # e.g. icate/ite) it means draws x obs with the chain axis simply missing,
  # not draws x chains as it does for a scalar `type` (pate/sate/cate/sigma).
  # Restore the dropped chain axis as a leading singleton dimension - the
  # ndim == 3 branch below expects [chain, draws, obs] (array_to_mcmclist()
  # is called with chain = dimension 1) - so both cases go through the same
  # (correct, .row-producing) code path. Checked in
  # tests/testthat/test-bartcause.R.
  if (ndim == 2 && type %in% bartcause_per_unit_types) {
    sample_array <- aperm(array(sample_array, dim = c(dim(sample_array), 1)), c(3, 1, 2))
    ndim <- 3
  }
  if(ndim == 2){
    draws <- matrix_to_mcmclist(sample_array, 2, 1) %>%
      tidybayes::tidy_draws() %>%
      dplyr::rename({{type}} := `var1`)
  } else if(ndim == 3) {
    draws <- array_to_mcmclist(sample_array, 2, 3, 1) %>%
      tidybayes::tidy_draws() %>%
      tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = type) %>%
      dplyr::mutate(.row = as.integer(gsub("var", "", .row))) %>%
      dplyr::group_by(.row)
  } else {
    stop("Problem with type argument, sample array must be dimension 2 or 3")
  }

  return(draws)

}
