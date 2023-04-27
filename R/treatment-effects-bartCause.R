
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

  stop("Not currently implemented, use summary(model,...) see ?summary.bartcFit")

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

  commonsupport <- tibble(.row = 1:length(refitmodel$commonSup.sub), supported = refitmodel$commonSup.sub)


  predict(refitmodel, type = "mu", newdata = data.frame(p.score = refitmodel$p.score))

  # tidy_draws uses extract which gets the average treatment effect (not per individual)
  # see below...
  tedf <- tidy_draws(refitmodel, type = "icate", fitstage = "response") %>%
    left_join(tidy_draws(refitmodel, type = "ite", fitstage = "response"),
              by = join_by(.chain, .iteration, .draw, .row)) %>%
    left_join(commonsupport, by = join_by(.row))

  # summary.bartcFit calculates icate/ite with common support for the CIs it uses.
  # could add argument to output the icate/ite from thus














}
