#' Get variance draws from posterior of BART models
#'
#' Models from \code{BART}-package include warm-up and skipped MCMC draws.
#'
#' @param model A model from a supported package.
#' @param value The name of the output column for variance parameter; default \code{".sigma_sq"}.
#' @param ... Additional arguments.
#'
#' @return A tidy data frame (tibble) with draws of variance parameter
#'
#' @export
variance_draws <- function(model, value = ".sigma_sq", ...){

  UseMethod("variance_draws")

}

#' @export
variance_draws.wbart <- function(model, value = ".sigma_sq", ...){

  sigma_draws <- model$sigma

  dplyr::tibble(.chain = NA_integer_,
                .iteration = NA_integer_,
                .draw =  1:length(sigma_draws),
                !!value := sigma_draws^2)

}

#' @export
variance_draws.bartMachine <- function(model, value = ".sigma_sq", ...){

  sigma2_draws <- bartMachine::get_sigsqs(model)

  dplyr::tibble(.chain = NA_integer_,
                .iteration = NA_integer_,
                .draw =  1:length(sigma2_draws),
                !!value := sigma2_draws)

}

