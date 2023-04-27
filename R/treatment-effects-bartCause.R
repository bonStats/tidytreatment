
#' Get treatment effect draws from bartcFit posterior
#'
#' CTE = Conditional Treatment Effects (or CATE, the average effects)
#' \code{newdata} specifies the conditions, if unspecified it defaults to the original data.
#'
#' @inheritParams treatment_effects
#'
#' @return A tidy data frame (tibble) with treatment effect values.
#' @export
#'
treatment_effects.bartcFit <- function(model, treatment, newdata, subset = "all", common_support_method, cutoff, ...) {


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



  newmodel <- bartCause::refit(model, newresp = NULL, commonSup.rule = commonSup.rule, commonSup.cut = commonSup.cut)


             "icate"












}
