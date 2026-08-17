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
variance_draws <- function(model, value = ".sigma_sq", ...) {
  UseMethod("variance_draws")
}

#' @export
variance_draws.wbart <- function(model, value = ".sigma_sq", ...) {
  # model$sigma: plain vector for a single chain, [samples x mc.cores] matrix
  # for mc.wbart()-combined chains - as.vector() flattens column-major, i.e.
  # chain-major, matching bart_chain_iteration_index()'s convention.
  sigma_draws <- if (is.matrix(model$sigma)) as.vector(model$sigma) else model$sigma
  chain_index <- bart_chain_iteration_index(model, length(sigma_draws))

  dplyr::tibble(
    .chain = chain_index$chain,
    .iteration = chain_index$iteration,
    .draw = seq_along(sigma_draws),
    !!value := sigma_draws^2
  )
}

#' @export
variance_draws.bartMachine <- function(model, value = ".sigma_sq", ...) {
  sigma2_draws <- bartMachine::get_sigsqs(model)

  dplyr::tibble(
    .chain = NA_integer_,
    .iteration = NA_integer_,
    .draw = 1:length(sigma2_draws),
    !!value := sigma2_draws
  )
}
