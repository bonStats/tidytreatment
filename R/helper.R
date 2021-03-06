#' Check if a model class has required generic methods for tidytreatment functions.
#'
#' @param model Model to be checked.
#'
#' @return Boolean
#' @export
#'
has_tidytreatment_methods <- function(model) {
  all(
    c("fitted_draws", "model.matrix") %in% attr(utils::methods(class = class(model)), "info")$generic
  )
}


is_01_integer_vector <- function(x) {
  class(x) == "integer" & all(x %in% c(0, 1))
}

has_installed_package <- function(package) {
  length(find.package(package, quiet = TRUE)) >= 1
}

has_method_str <- function(cl, method) {
  mth <- methods(class = cl)
  method %in% attr(mth, "info")[, "generic"]
}

check_method <- function(x, method, helper = "") {
  x_cl <- class(x)
  if (!has_method_str(x_cl, method)) {
    stop("Object of class '", x_cl, "' does not have method '", method, "'.\n", helper, call. = FALSE)
  }
}
