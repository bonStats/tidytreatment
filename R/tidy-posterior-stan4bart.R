
# helper functions
array_to_mcmclist <- function(arr, sample, variable, chain){

  stopifnot(sort(c(sample, variable, chain)) == 1:3)

  if(sample > variable){
    tt <- t
  } else {
    tt <- identity
  }

  chains <- dim(arr)[[chain]]
  if(chain == 1){
    # modeled from: https://github.com/mjskay/tidybayes/blob/199f13b759b6c93e9277fbd49a1728434cce7700/R/tidy_draws.R#L179
    ss <- coda::as.mcmc.list(lapply(seq_len(chains), function(ch) coda::as.mcmc(tt(arr[ch, , ]))))
  } else if(chain == 2) {
    ss <- coda::as.mcmc.list(lapply(seq_len(chains), function(ch) coda::as.mcmc(tt(arr[ , ch,]))))
  } else if(chain == 3) {
    ss <- coda::as.mcmc.list(lapply(seq_len(chains), function(ch) coda::as.mcmc(tt(arr[ , ,ch]))))
  }

  return(ss)

}

matrix_to_mcmclist <- function(arr, sample, chain){

  stopifnot(sort(c(sample, chain)) == 1:2)

  chains <- dim(arr)[[chain]]
  if(chain == 1){
    ss <- coda::as.mcmc.list(lapply(seq_len(chains), function(ch) coda::as.mcmc(matrix(arr[ch, ], ncol = 1))))
  } else if(chain == 2) {
    ss <- coda::as.mcmc.list(lapply(seq_len(chains), function(ch) coda::as.mcmc(matrix(arr[ ,ch], ncol = 1))))
  }

  return(ss)

}

#' Get expected prediction draws from posterior of \code{stan4bart}-package models
#'
#' Typically referred to as fitted value draws on response scale, where appropriate.
#'
#' @param object A \code{stan4bartFit} object.
#'
#' @param newdata Data frame to generate predictions from [optional].
#' @param ... Additional arguments passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#' @param re_formula If NULL (default), include all group-level effects; if NA, include no group-level effects.
#'
#' @export
epred_draws.stan4bartFit = function(
    object, newdata, ...,
    value = ".epred", re_formula = NULL
) {

  if( (length(re_formula) > 1) || ( !is.null(re_formula) && !is.na(re_formula) ) ) warning("re_formula should be NULL or NA for stan4bartFit object. No random effects included.")
  # re_formula = NULL --> random effects == sample_new_levels = T
  # re_formula = NA --> no random effects == sample_new_levels = F

  if(missing(newdata)){
    sample_array <- dbarts::extract(object = object,
      type = "ev", sample_new_levels = is.null(re_formula),
      combine_chains = FALSE, ...)
  } else {
    sample_array <- predict(object = object,
      newdata = newdata, type = "ev", sample_new_levels = is.null(re_formula),
      combine_chains = FALSE, ...)
  }

  mcmc_list <- array_to_mcmclist(sample_array, 2, 1, 3)

  array_to_mcmclist(sample_array, 2, 1, 3) %>%
    tidybayes::tidy_draws() %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = value) %>%
    dplyr::mutate(.row = as.integer(gsub("var", "", .row))) %>%
    dplyr::group_by(.row)

}

#' Get prediction draws from posterior of \code{stan4bart}-package models
#'
#' @param object A \code{stan4bartFit} object.
#'
#' @param newdata Data frame to generate predictions from [optional].
#' @param ... Additional arguments passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#' @param re_formula If NULL (default), include all group-level effects; if NA, include no group-level effects.
#'
#' @export
predicted_draws.stan4bartFit = function( # code from epred_draws.stan4bartFit, consider combining.
    object, newdata, ...,
    value = ".prediction", re_formula = NULL
) {

  if( (length(re_formula) > 1) || ( !is.null(re_formula) && !is.na(re_formula) ) ) warning("re_formula should be NULL or NA for stan4bartFit object. No random effects included.")
  # re_formula = NULL --> random effects == sample_new_levels = T
  # re_formula = NA --> no random effects == sample_new_levels = F

  if(missing(newdata)){
    sample_array <- dbarts::extract(object = object,
                                    type = "ppd", sample_new_levels = is.null(re_formula),
                                    combine_chains = FALSE, ...)
  } else {
    sample_array <- predict(object = object,
                            newdata = newdata, type = "ppd", sample_new_levels = is.null(re_formula),
                            combine_chains = FALSE, ...)
  }

  mcmc_list <- array_to_mcmclist(sample_array, 2, 1, 3)

  array_to_mcmclist(sample_array, 2, 1, 3) %>%
    tidybayes::tidy_draws() %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = value) %>%
    dplyr::mutate(.row = as.integer(gsub("var", "", .row))) %>%
    dplyr::group_by(.row)

}

#' Get expected prediction draws (on linear scale) from posterior of \code{stan4bart}-package models
#'
#' Typically referred to as fitted value draws on linear scale, where appropriate.
#'
#' @param object A \code{stan4bartFit} object.
#'
#' @param newdata Data frame to generate predictions from [optional].
#' @param ... Additional arguments passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#' @param re_formula If NULL (default), include all group-level effects; if NA, include no group-level effects.
#'
#' @export
linpred_draws.stan4bartFit = function(
    object, newdata, ...,
    value = ".linpred", re_formula = NULL
) {

  if( (length(re_formula) > 1) || ( !is.null(re_formula) && !is.na(re_formula) ) ) warning("re_formula should be NULL or NA for stan4bartFit object. No random effects included.")
  # re_formula = NULL --> random effects == sample_new_levels = T
  # re_formula = NA --> no random effects == sample_new_levels = F

  is_bernoulli <- object$family$family == "binomial"

  if(!is_bernoulli){
    if(missing(newdata)){
      draws <- tidybayes::epred_draws(object, ..., value = value, re_formula = re_formula)
    } else {
      draws <- tidybayes::epred_draws(object, newdata = newdata, ..., value = value, re_formula = re_formula)
    }
    return(draws)
  }

  # else ... is_bernoulli == TRUE
  if(missing(newdata)){
    sample_array <- Reduce("+", lapply(
      c("indiv.fixef", "indiv.ranef", "indiv.bart"),
      function(type) dbarts::extract(
        object = object, type = type, sample_new_levels = is.null(re_formula),
        combine_chains = FALSE, ...)
    )
    )
    # can't find generic version of linpred...
    # see: https://github.com/vdorie/stan4bart/blob/2e474c4dbabd583fbaced3c89c05fdf06e963ebc/R/generics.R#L419
  } else {
    sample_array <- Reduce("+", lapply(
      c("indiv.fixef", "indiv.ranef", "indiv.bart"),
      function(type) predict(
        object = object, newdata = newdata, type = type, sample_new_levels = is.null(re_formula),
        combine_chains = FALSE, ...)
    )
    )
  }

  array_to_mcmclist(sample_array, 2, 1, 3) %>%
    tidybayes::tidy_draws() %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = value) %>%
    dplyr::mutate(.row = as.integer(gsub("var", "", .row))) %>%
    dplyr::group_by(.row)

}

#' @export
tidy_draws.stan4bartFit = function(model, ...) {
  mcmc_list = array_to_mcmclist(as.array(model), 1, 3, 2)
  draws = tidybayes::tidy_draws(mcmc_list, ...)
  return(draws)
}



