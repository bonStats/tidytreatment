
#' Get (individual) treatment effect draws from bartcFit posterior
#'
#' CTE = Conditional Treatment Effects (usually used to generate (C)ATE or ATT)
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#' Assumes treated column is either a integer column of 1's (treated) and 0's (nontreated) or logical indicating treatment if TRUE.
#'
#' @param model A supported Bayesian model fit that can provide fits and predictions.
#' @param treatment Not used. Treatment variable specified by \code{bartcFit} object.
#' @param newdata Not used. extracts treatment effects already calculated by \code{bartcFit} object.
#' @param subset Either "treated", "nontreated", or "all". Default is "all".
#' @param common_support_method Either "sd", or "chisq". Default is unspecified, and no common support calculation is done.
#' @param cutoff Cutoff for common support (if in use).
#' @param ... Arguments to be passed to \code{tidybayes::fitted_draws} typically scale for \code{BART} models.

#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
treatment_effects.bartcFit <- function(model, treatment = NULL, newdata = NULL, subset = "all", common_support_method, cutoff, ...) {

  stopifnot(is.null(treatment), is.null(newdata))

  # update specified common support arguments
  if(missing(common_support_method)){
    commonSup.rule <- "none"
    commonSup.cut <- NA_real_
    if(!missing(cutoff)) warning("Argument cutoff ignored as common_support_method unspecified.")
  } else {
    commonSup.rule <- common_support_method
    if(missing(cutoff)){
      commonSup.cut = switch(common_support_method,
                             sd = 1,
                             chisq = 0.05
                             )
      warning("Default value for cutoff used.")
    } else {
      commonSup.cut = cutoff
    }
  }

  refitmodel <- bartCause::refit(model, newresp = NULL, commonSup.rule = commonSup.rule, commonSup.cut = commonSup.cut)

  # extract treatment effect

  rowinfo <- dplyr::tibble(.row = 1:length(refitmodel$commonSup.sub), treated = model$trt)
  if(commonSup.rule != "none"){
    rowinfo <- rowinfo %>% dplyr::mutate(supported = refitmodel$commonSup.sub)
  }

  te_df <- tidy_draws(refitmodel, type = "icate", fitstage = "response", sample = "all") %>%
    dplyr::left_join(tidy_draws(refitmodel, type = "ite", fitstage = "response"),
              by = dplyr::join_by(".chain", ".iteration", ".draw", ".row")) %>%
    dplyr::left_join(rowinfo, by = dplyr::join_by(.row))

  if(subset == "treated"){
    te_df <- te_df %>% dplyr::filter(!!as.symbol("treated") == 1)
  } else if (subset == "nontreated") {
    te_df <- te_df %>% dplyr::filter(!!as.symbol("treated") == 0)
  }

  return(te_df)

}
