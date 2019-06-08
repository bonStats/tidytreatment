#' Get treatment effect draws from posterior
#'
#' CTE = Conditional Treatment Effects (or CATE, the average effects)
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment A character string specifying the name of the treatment variable.
#' @param newdata Data frame to generate fitted values from. If omitted, defaults to the data used to fit the model.
#'
#' @return A tidy data frame (tibble) with CATE values.
#' @export
#'
treatment_effects <- function(model, treatment, newdata){

  stopifnot(
    !missing(treatment),
    is.character(treatment),
    length(treatment) == 1
  )

  if(missing(newdata)){
    newdata <- model.matrix(model)
  }

  stopifnot(
    treatment %in% colnames(newdata),
    is.data.frame(newdata)
  )

  stopifnot(
    is_01_integer_vector(newdata[,treatment]) | is.logical(newdata[,treatment])
  )

  treatment_class <- class( newdata[,treatment] )

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
                   newdata = mutate(newdata, !!treatment := treatment_on),
                   include_newdata = F),
      fitted_draws(model = model, value = "off",
                   newdata = mutate(newdata, !!treatment := treatment_off),
                   include_newdata = F),
      by = c(".row", ".chain", ".iteration", ".draw")
    )

  posterior_treatment <- select(
    mutate(posterior_treatment, cte = on - off),
    -on, -off)

  return(posterior_treatment)

}
