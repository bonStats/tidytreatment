#' Get CATE draws from posterior of \code{bartMachine} model
#'
#' CATE = Conditional Average Treatment Effects
#'
#' @param model A \code{bartMachine} model.
#' @param treatment A character string specifying the name of the treatment variable.
#'
#' @return A tidy data frame (tibble) with CATE values.
#' @export
#'
posterior_cate_from_sample <- function(model, treatment){

  stopifnot(
    !missing(treatment),
    is.character(treatment),
    length(treatment) == 1
  )

  model_data <- model.matrix(model)

  stopifnot(
    treatment %in% colnames(model_data)
  )

  stopifnot(
    is_01_integer_vector(model_data[,treatment]) | is.logical(model_data[,treatment])
  )

  treatment_class <- class( model_data[,treatment] )

  if(treatment_class == "integer"){
    treatment_on <- 1L
    treatment_off <- 0L
  } else if(treatment_class == "logical"){
    treatment_on <- TRUE
    treatment_off <- FALSE
  }

  posterior_treatment <-
    left_join(
      fitted_draws(model = model, value = "on",
                   newdata = mutate(model_data, !!treatment := treatment_on),
                   include_newdata = F),
      fitted_draws(model = model, value = "off",
                   newdata = mutate(model_data, !!treatment := treatment_off),
                   include_newdata = F),
      by = c(".row", ".chain", ".iteration", ".draw")
    )

  posterior_treatment <- select(
    mutate(posterior_treatment, cte = on - off),
    -on, -off)

  return(posterior_treatment)

}
