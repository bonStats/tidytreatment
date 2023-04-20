
#' @export
tidy_draws.stan4bartFit = function(model, ...) {
  # modelled from: https://github.com/mjskay/tidybayes/blob/199f13b759b6c93e9277fbd49a1728434cce7700/R/tidy_draws.R#L179
  sample_matrix = as.array(model)
  n_chain = dim(sample_matrix)[[2]]
  mcmc_list = coda::as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(sample_matrix[, chain, ])))
  draws = tidybayes::tidy_draws(mcmc_list, ...)

  return(draws)
}

#' @export
tidy_draws.bartcFit = function(model, stage = "rsp", ...) {

  if(stage == "rsp"){
    draws = tidy_draws(model$fit.rsp, ...)
  } else {
    draws = tidy_draws(model$fit.trt, ...)
  }

  return(draws)
}

#' @export
epred_draws.stan4bartFit = function(
    object, newdata, ...,
    value = ".epred", ndraws = NULL, seed = NULL, re_formula = NULL,
    category = ".category", dpar = NULL
) {

  if(!missing(ndraws)) warning("ndraws not in use with stan4bartFit object")
  if(!missing(seed)) warning("seed not in use with stan4bartFit object")
  if(!missing(category)) warning("category not in use with stan4bartFit object")
  if(!missing(dpar)) warning("dpar not in use with stan4bartFit object")

  # re_formula = NULL --> random effects == sample_new_levels = T
  # re_formula = NA --> no random effects == sample_new_levels = F

  if(missing(newdata)){
    sample_matrix <- stan4bart:::extract.stan4bartFit(
      object = object, type = "ev", sample_new_levels = is.null(re_formula),
      combine_chains = FALSE, ...)
  } else {
    stop("Methods using newdata not yet implemented")
  }

  n_chain = dim(sample_matrix)[[3]]
  mcmc_list = coda::as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(t(sample_matrix[, , chain]))))

  tidy_draws(mcmc_list) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = value) %>%
    mutate(.row = as.integer(gsub("var", "", .row))) %>%
    dplyr::group_by(.row)

}

#' @export
linpred_draws.stan4bartFit = function(
    object, newdata, ...,
    value = ".linpred", ndraws = NULL, seed = NULL, re_formula = NULL,
    category = ".category", dpar = NULL
) {

  if(!missing(ndraws)) warning("ndraws not in use with stan4bartFit object")
  if(!missing(seed)) warning("seed not in use with stan4bartFit object")
  if(!missing(category)) warning("category not in use with stan4bartFit object")
  if(!missing(dpar)) warning("dpar not in use with stan4bartFit object")

  # re_formula = NULL --> random effects == sample_new_levels = T
  # re_formula = NA --> no random effects == sample_new_levels = F

  is_bernoulli <- object$family$family == "binomial"

  if(missing(newdata)){

    if(!is_bernoulli){
      draws <- epred_draws(object, ...,
                  value = value, ndraws = ndraws, seed = seed, re_formula = re_formula,
                  category = category, dpar = dpar)
      return(draws)
    } else {
      sample_matrix <- Reduce("+", sapply(
          c("indiv.fixef", "indiv.ranef", "indiv.bart"),
          function(type) stan4bart:::extract.stan4bartFit(
          object = object, type = type, sample_new_levels = is.null(re_formula),
          combine_chains = FALSE, ...)
        )
      ) # see: https://github.com/vdorie/stan4bart/blob/2e474c4dbabd583fbaced3c89c05fdf06e963ebc/R/generics.R#L419
    }


  } else {
    stop("Methods using newdata not yet implemented")
  }

  n_chain = dim(sample_matrix)[[3]]
  mcmc_list = coda::as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(t(sample_matrix[, , chain]))))

  draws <- tidy_draws(mcmc_list) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("var"), names_to = ".row", values_to = value) %>%
    mutate(.row = as.integer(gsub("var", "", .row))) %>%
    dplyr::group_by(.row)

  return(draws)

}







#' Get fitted draws from posterior of \code{stan4bart}-package models
#'
#' @param model A model from \code{stan4bart} package.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#' @param value The name of the output column for \code{fitted_draws}; default \code{".value"}.
#' @param include_newdata Should the newdata be included in the tibble?
#' @param include_sigsqs Should the posterior sigma-squared draw be included?
#' @param scale Should the fitted values be on the real, probit or logit scale?
#' @param ... Arguments to pass to \code{predict} or \code{fitted} (e.g. \code{stan4bart:::extract}).
#'
#' @return A tidy data frame (tibble) with fitted values.
#'
fitted_draws_stan4bart <- function(model, newdata = NULL, value = ".value", ..., include_newdata = TRUE, include_sigsqs = FALSE, scale = "real") {
  stopifnot(has_installed_package("stan4bart"))

  if (is.null(newdata) & include_newdata) { #CHECK if we can add new data on
    stop("For models from BART package 'newdata'
          must be specified if 'include_newdata = TRUE'.")
  }

  stopifnot(
    is.character(value),
    is.logical(include_newdata),
    is.logical(include_sigsqs)
  )

  use_scale <- match.arg(scale,
                         c("real", "prob"),
                         several.ok = F
  )

  type <- switch(use_scale,
                 real = 'ev',
                 prod = 'ppd')

  # order for columns in output
  col_order <- c(".row", ".chain", ".iteration", ".draw", value)

  #HERE check extract type etc...

  if (!(missing(newdata) | is.null(newdata))) {
    posterior <- extract(model, newdata = newdata, type = type, ...)
  } else {
    posterior <- extract(model, type = type, ...)
  }

  if (use_scale == "prob" & "lbart" %in% class(model)) posterior <- stats::plogis(posterior)
  if (use_scale == "prob" & "pbart" %in% class(model)) posterior <- stats::pnorm(posterior)

  # bind newdata with fitted, wide format
  out <- dplyr::bind_cols(
    if (include_newdata) dplyr::as_tibble(newdata) else NULL,
    dplyr::as_tibble(t(posterior), .name_repair = function(names) {
      paste0(".col_iter", as.character(1:length(names)))
    }),
    .row = 1:ncol(posterior)
  )

  # convert to long format
  out <- tidyr::gather(out, key = ".draw", value = !!value, dplyr::starts_with(".col_iter"))

  # add variables to keep to generic standard, remove string in
  out <- dplyr::mutate(out, .chain = NA_integer_, .iteration = NA_integer_, .draw = as.integer(gsub(pattern = ".col_iter", replacement = "", x = .data$.draw)))

  # include sigma^2 if needed
  if (include_sigsqs) {
    sigsq <- dplyr::bind_cols(
      .draw = 1:length(model$sigma),
      sigsq = model$sigma^2
    )

    out <- dplyr::left_join(out, sigsq, by = ".draw")

    col_order <- c(col_order, "sigsq")
  }

  # rearrange
  out <- dplyr::select(out, -!!col_order, !!col_order)

  # group
  row_groups <- names(out)[!names(out) %in% col_order[col_order != ".row"]]

  out <- dplyr::group_by(out, dplyr::across(row_groups))

  return(out)
}

