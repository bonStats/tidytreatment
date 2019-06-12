#' Simulate data with scenario from Su and Hill (2013)
#'
#' Under construction.
#'
#' @param n
#'
#' @return data.frame with data.
#' @export
simulate_su_hill_data <- function(n){

  # not generic yet, but should make it so
  gam <- 1
  ome <- 1
  tau <- 4

  coef_assignment <- c( rep(0, times = 5), 0.4, 0.2, 0.4, 0.2, 0.4, 0.2, rep(0, times = 10))

  invlogit <- function(x){ exp(x) / ( 1 + exp(x) )  }

  # simulate data
  X <- as.data.frame( matrix(rnorm(n * 10, mean = 0, sd = 1), ncol = 10) )
  colnames(X) <- paste0("x",1:10)
  model_matrix <- as.matrix( model.frame(~ x1 + x2 + I(x1^2) + I(x2^2) + I(x2*x6) +
                x5 + x6 + x7 + x8 + x9 + x10 +
                I(x5^2) + I(x6^2) + I(x5*x6) + I(x5*x6*x7) +
                I(x7^2) + I(x7^3) + I(x8^2) + I(x7*x8) + I(x9^2) + I(x9*x10)
                , data = X) )


  # assign to treatment
  p <- invlogit(ome + model_matrix %*% coef_assignment)

  z <- rbinom(n = n, size = 1, prob = p)

  # response surfaces
  coef_treatment <- c(rep(0,times = 5), 0.5, 0, 2, 0, 0.5, 2, 0.4, 0.8, 0, 0, 0.5, 0, 0.5, 0, 0.5, 0.7)
  coef_non_treatment <- c(rep(0,times = 5), 0.5, 0, 1, 0.5, 0, 0.8, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0)

  #(factor not in su and hill...)
  extra_factor1 <- rbinom(n = n, size = 1, prob = 0.4)
  extra_factor2 <- 2L * rbinom(n = n, size = 1, prob = 0.4) + rbinom(n = n, size = 1, prob = 0.4)

  mean_y <- ifelse(z == 1, model_matrix %*% coef_treatment + tau + 0.6 * extra_factor1 + 2*extra_factor2,  model_matrix %*% coef_non_treatment - 4*extra_factor2)

  y <- rnorm(n = n, mean = mean_y, sd = 1)

  return(cbind(data.frame(y=y,z=z, x11 = extra_factor1, x12 = extra_factor2), X))

}
