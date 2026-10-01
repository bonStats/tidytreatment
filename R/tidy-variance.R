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
  # model$sigma prepends nskip burn-in draws to the ndpost kept draws (see
  # bart_sigma_aligned()'s own header comment in tidy-posterior-BART.R) -
  # n_total must come from yhat.train (always post-burn-in only), not from
  # sigma's own length, or burn-in silently leaks into the returned draws
  # and their .chain/.iteration labels. Mirrors tidy_draws.wbart()'s own
  # (correct) extraction exactly.
  n_total <- nrow(model$yhat.train)
  chain_index <- bart_chain_iteration_index(model, n_total)
  sigma_draws <- bart_sigma_aligned(model, n_total)

  dplyr::tibble(
    .chain = chain_index$chain,
    .iteration = chain_index$iteration,
    .draw = seq_len(n_total),
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
