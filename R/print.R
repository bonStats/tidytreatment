#' @export
print.wbart <- function(x, ...){
  cmps <- paste0("\t$", names(x), collapse = "\n")
  cat("\nBART::wbart with",length(x$yhat.train.mean),"samples\n")
  cat("components:\n", cmps)
}

#' @export
print.pbart <- function(x, ...){
  cmps <- paste0("\t$", names(x), collapse = "\n")
  cat("\nBART::pbart with",length(x$yhat.train.mean),"samples\n")
  cat("components:\n", cmps)
}

#' @export
print.lbart <- function(x, ...){
  cmps <- paste0("\t$", names(x), collapse = "\n")
  cat("\nBART:lbart with",length(x$yhat.train.mean),"samples\n")
  cat("components:\n", cmps)
}

#' @export
print.mbart <- function(x, ...){
  cmps <- paste0("\t$", names(x), collapse = "\n")
  cat("\nBART::mbart with",length(x$yhat.train.mean),"samples\n")
  cat("components:\n", cmps)
}

#' @export
print.mbart2 <- function(x, ...){
  cmps <- paste0("\t$", names(x), collapse = "\n")
  cat("\nBART::mbart2 with",length(x$yhat.train.mean),"samples\n")
  cat("components:\n", cmps)
}
