#' @export
print.wbart <- function(x, ...){
  cmps <- paste0("\t$", names(x), collapse = "\n")
  cat("\nBART::wbart with",length(x$yhat.train.mean),"samples\n")
  cat("components:\n", cmps)
}
