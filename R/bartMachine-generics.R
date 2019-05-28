#' @export
model.matrix.bartMachine <- function(model, ...){

  model$X

}

#' @export
model.frame.bartMachine <- function(model, ...){

  cbind(y = model$y, model$X)

}
