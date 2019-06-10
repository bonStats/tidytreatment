#' Evaluate if observations have common support.
#'
#' Cite Hill and Su 2013
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param method Method to use in determining common support. 'chisq', ...
#' @param cutoff Cutoff point to use for method.
#'
#' @return Tibble with a row for each observation and a column indicating whether common support exists.
#' @export
#'
has_common_support <- function(model, treatment, method, cutoff){

  modeldata <- stats::model.matrix(model)

  stopifnot(
    treatment %in% colnames(modeldata),
    is.data.frame(modeldata)
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

  posterior_obs_cf_sd <- dplyr::summarise(
    fitted_with_counter_factual_draws(model = model, treatment = treatment),
      sd_observed = stats::sd(observed),
      sd_cfactual = stats::sd(cfactual)
    )

  common_support_cutoff <- switch(method,
                                  chisq = common_support_chisq_method,
                                  common_support_default
                                  )

  dplyr::mutate(posterior_obs_cf_sd,
         common_support =
           common_support_cutoff(sd_obs = sd_observed,
                                 sd_cf = sd_cfactual,
                                 cutoff = cutoff)
         )


}

common_support_chisq_method <- function(sd_obs, sd_cf, cutoff){

  (sd_cf/sd_obs)^2 < stats::qchisq(1 - cutoff, 1)

}

common_support_default <- function(sd_obs, sd_cf, cutoff){

  rep(NA, times = length(sd_obs))

}
