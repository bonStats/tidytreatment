#' Evaluate if observations have common support.
#'
#' The common support identification methods are based on Hill and Su (2013), The Annals of Applied Statistics, 7(3).
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param method Method to use in determining common support. 'chisq', or 'sd'.
#' @param cutoff Cutoff point to use for method.
#'
#' @return Tibble with a row for each observation and a column indicating whether common support exists.
#' @export
#'
has_common_support <- function(model, treatment, method, cutoff){

  modeldata <- stats::model.matrix(model)

  stopifnot(
    treatment %in% colnames(modeldata),
    is.data.frame(modeldata),
    !missing(cutoff)
  )

  stopifnot(
    is_01_integer_vector(modeldata[,treatment]) | is.logical(modeldata[,treatment])
  )

  treatment_class <- class( modeldata[,treatment] )

  if(treatment_class == "integer"){
    counter_factual <- function(x){ 1L - x }
  } else if(treatment_class == "logical"){
    counter_factual <- function(x){ !x }
  }

  calc_common_support_from_fitted_and_cf(
    fitted_and_cf = fitted_with_counter_factual_draws(model = model, treatment = treatment, subset = "all"),
    modeldata = modeldata,
    treatment = treatment,
    method = method,
    cutoff = cutoff
    )

}

calc_common_support_from_fitted_and_cf <- function(fitted_and_cf, modeldata, treatment, method, cutoff){

  posterior_obs_cf_sd <- dplyr::summarise(
    fitted_and_cf,
    sd_observed = stats::sd(observed),
    sd_cfactual = stats::sd(cfactual)
  )

  common_support_cutoff <- switch(method,
                                  sd = common_support_sd_method,
                                  chisq = common_support_chisq_method,
                                  common_support_default
  )

  dplyr::mutate(posterior_obs_cf_sd,
                common_support =
                  common_support_cutoff(sd_obs = sd_observed,
                                        sd_cf = sd_cfactual,
                                        cutoff = cutoff,
                                        treatment = modeldata[posterior_obs_cf_sd$.row,treatment])
  )


}


common_support_chisq_method <- function(sd_obs, sd_cf, cutoff, ...){

  # the sd of the counterfactual divided by the sd of
  # the actual observation is approx Chi^2.
  (sd_cf/sd_obs)^2 < stats::qchisq(1 - cutoff, 1)

}

common_support_sd_method <- function(sd_obs, sd_cf, treatment, ...){

  sd_obs_treated <- sd_obs[treatment == 1L]

  m_a <- max(sd_obs_treated)

  sd_cf < m_a + stats::sd(sd_obs_treated)

}

common_support_default <- function(sd_obs, sd_cf, cutoff){

  warning("Please specify common support 'method'.")
  rep(NA, times = length(sd_obs))

}
