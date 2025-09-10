
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
#'
#' @export
epred_draws.stan4bartFit = function(
    object, newdata, ...,
    value = ".epred"
) {

  if(missing(newdata)){
    sample_array <- dbarts::extract(object = object,
      type = "ev",
      combine_chains = FALSE, ...)
  } else {
    sample_array <- predict(object = object,
      newdata = newdata, type = "ev",
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
#' @param sample_new_levels logical; if TRUE, levels out of the training sample will have random effects drawn from their posterior predictive distribution. If FALSE, their random effects will be fixed to 0.
#'
#' @export
predicted_draws.stan4bartFit = function( # code from epred_draws.stan4bartFit, consider combining.
    object, newdata, ...,
    value = ".prediction", sample_new_levels = TRUE
) {

  if(missing(newdata)){
    sample_array <- dbarts::extract(object = object,
                                    type = "ppd", sample_new_levels = sample_new_levels,
                                    combine_chains = FALSE, ...)
  } else {
    sample_array <- predict(object = object,
                            newdata = newdata, type = "ppd", sample_new_levels = sample_new_levels,
                            combine_chains = FALSE, ...)
  }

  mcmc_list <- array_to_mcmclist(sample_array, 2, 1, 3)

  array_to_mcmclist(sample_array, 2, 1, 3) %>%
    tidybayes::tidy_draws() %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = value) %>%
    dplyr::mutate(.row = as.integer(gsub("var", "", .row))) %>%
    dplyr::group_by(.row)

}

#' Get linear predictor draws from posterior of \code{stan4bart}-package models
#'
#' Typically referred to as fitted value draws on linear scale, where appropriate.
#'
#' @param object A \code{stan4bartFit} object.
#'
#' @param newdata Data frame to generate predictions from [optional].
#' @param ... Additional arguments passed to the underlying prediction method for the type of model given.
#' @param value The name of the output column.
#'
#' @export
linpred_draws.stan4bartFit = function(
    object, newdata, ...,
    value = ".linpred"
) {

  is_bernoulli <- object$family$family == "binomial"

  if(!is_bernoulli){
    if(missing(newdata)){
      draws <- tidybayes::epred_draws(object, ..., value = value)
    } else {
      draws <- tidybayes::epred_draws(object, newdata = newdata, ..., value = value)
    }
    return(draws)
  }

  # else ... is_bernoulli == TRUE
  if(missing(newdata)){
    sample_array <- Reduce("+", lapply(
      c("indiv.fixef", "indiv.ranef", "indiv.bart"),
      function(type) dbarts::extract(
        object = object, type = type,
        combine_chains = FALSE, ...)
    )
    )
    # can't find generic version of linpred...
    # see: https://github.com/vdorie/stan4bart/blob/2e474c4dbabd583fbaced3c89c05fdf06e963ebc/R/generics.R#L419
  } else {
    sample_array <- Reduce("+", lapply(
      c("indiv.fixef", "indiv.ranef", "indiv.bart"),
      function(type) predict(
        object = object, newdata = newdata, type = type,
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



