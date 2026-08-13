#' Check if a model class has required generic methods for tidytreatment functions.
#'
#' @param model Model to be checked.
#'
#' @return Boolean
#' @export
#'
has_tidytreatment_methods <- function(model) {
  cl <- class(model)[1]
  has_method_str(cl, "epred_draws") && has_method_str(cl, "model.matrix")
}


is_01_integer_vector <- function(x) {
  class(x) == "integer" & all(x %in% c(0, 1))
}

has_installed_package <- function(package) {
  length(find.package(package, quiet = TRUE)) >= 1
}

has_method_str <- function(cl, method) {
  # utils::methods(class=)/utils::getS3method() can only resolve a generic
  # that is reachable (as a plain object) from the calling environment's
  # search path. Generics imported (but not re-exported/attached) by
  # tidytreatment - e.g. tidybayes::epred_draws - are invisible to that
  # lookup once tidytreatment is loaded normally (library()/R CMD check),
  # even though the S3 method itself is correctly registered. Searching the
  # S3 method table of every loaded namespace avoids that dependency on
  # search-path visibility.
  method_name <- paste0(method, ".", cl)
  for (pkg in loadedNamespaces()) {
    s3_table <- tryCatch(
      get(".__S3MethodsTable__.", envir = asNamespace(pkg), inherits = FALSE),
      error = function(e) NULL
    )
    if (!is.null(s3_table) && exists(method_name, envir = s3_table, inherits = FALSE)) {
      return(TRUE)
    }
  }
  FALSE
}

check_method <- function(x, method, helper = "") {
  x_cl <- class(x)
  if (!has_method_str(x_cl, method)) {
    stop("Object of class '", x_cl, "' does not have method '", method, "'.\n", helper, call. = FALSE)
  }
}
