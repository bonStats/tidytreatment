#' Check if a model class has required generic methods for tidytreatment functions.
#'
#' @param model Model to be checked.
#'
#' @return Boolean
#' @export
#'
has_tidytreatment_methods <- function(model){

  all(
    c("fitted_draws", "model.matrix") %in% attr(utils::methods(class = class(model)), "info")$generic
  )

}


is_01_integer_vector <- function(x){

  class(x) == "integer" & all( x %in% c(0,1) )

}
