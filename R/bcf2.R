#' bcf 2
#'
#' @inheritParams bcf::bcf
#'
#' @export
#'
bcf2 <- function (y, z, x_control, x_moderate = x_control, pihat, nburn,
          nsim, nthin = 1, update_interval = 100, ntree_control = 200,
          sd_control = 2 * sd(y), base_control = 0.95, power_control = 2,
          ntree_moderate = 50, sd_moderate = sd(y), base_moderate = 0.25,
          power_moderate = 3, nu = 3, lambda = NULL, sigq = 0.9, sighat = NULL,
          include_pi = "control", use_muscale = TRUE, use_tauscale = TRUE)
{
  pihat = as.matrix(pihat)
  if (!bcf:::.ident(length(y), length(z), nrow(x_control), nrow(x_moderate),
              nrow(pihat))) {
    stop("Data size mismatch. The following should all be equal:\n         length(y): ",
         length(y), "\n", "length(z): ", length(z), "\n",
         "nrow(x_control): ", nrow(x_control), "\n", "nrow(x_moderate): ",
         nrow(x_moderate), "\n", "nrow(pihat): ", nrow(pihat),
         "\n")
  }
  if (any(is.na(y)))
    stop("Missing values in y")
  if (any(is.na(z)))
    stop("Missing values in z")
  if (any(is.na(x_control)))
    stop("Missing values in x_control")
  if (any(is.na(x_moderate)))
    stop("Missing values in x_moderate")
  if (any(is.na(pihat)))
    stop("Missing values in pihat")
  if (any(!is.finite(y)))
    stop("Non-numeric values in y")
  if (any(!is.finite(z)))
    stop("Non-numeric values in z")
  if (any(!is.finite(x_control)))
    stop("Non-numeric values in x_control")
  if (any(!is.finite(x_moderate)))
    stop("Non-numeric values in x_moderate")
  if (any(!is.finite(pihat)))
    stop("Non-numeric values in pihat")
  if (!all(sort(unique(z)) == c(0, 1)))
    stop("z must be a vector of 0's and 1's, with at least one of each")
  if (length(unique(y)) < 5)
    warning("y appears to be discrete")
  if (nburn < 0)
    stop("nburn must be positive")
  if (nsim < 0)
    stop("nsim must be positive")
  if (nthin < 0)
    stop("nthin must be positive")
  if (nthin > nsim + 1)
    stop("nthin must be < nsim")
  if (nburn < 100)
    warning("A low (<100) value for nburn was supplied")
  x_c = matrix(x_control, ncol = ncol(x_control))
  x_m = matrix(x_moderate, ncol = ncol(x_moderate))
  if (include_pi == "both" | include_pi == "control") {
    x_c = cbind(x_control, pihat)
  }
  if (include_pi == "both" | include_pi == "moderate") {
    x_m = cbind(x_moderate, pihat)
  }
  cutpoint_list_c = lapply(1:ncol(x_c), function(i) bcf:::.cp_quantile(x_c[,i]))
  cutpoint_list_m = lapply(1:ncol(x_m), function(i) bcf:::.cp_quantile(x_m[,i]))
  yscale = scale(y)
  sdy = sd(y)
  muy = mean(y)
  if (is.null(lambda)) {
    if (is.null(sighat)) {
      lmf = lm(yscale ~ z + as.matrix(x_c))
      sighat = summary(lmf)$sigma
    }
    qchi = qchisq(1 - sigq, nu)
    lambda = (sighat * sighat * qchi)/nu
  }
  dir = tempdir()
  perm = order(z, decreasing = TRUE)
  fitbcf = bcf:::bcfoverparRcppClean(yscale[perm], z[perm], t(x_c[perm,
                                                            ]), t(x_m[perm, , drop = FALSE]), t(x_m[1, , drop = FALSE]),
                               cutpoint_list_c, cutpoint_list_m, random_des = matrix(1),
                               random_var = matrix(1), random_var_ix = matrix(1), random_var_df = 3,
                               nburn, nsim, nthin, ntree_moderate, ntree_control, lambda,
                               nu, con_sd = ifelse(abs(2 * sdy - sd_control) < 1e-06,
                                                   2, sd_control/sdy), mod_sd = ifelse(abs(sdy - sd_moderate) <
                                                                                         1e-06, 1, sd_moderate/sdy)/ifelse(use_tauscale, 0.674,
                                                                                                                           1), base_control, power_control, base_moderate, power_moderate,
                               "~/Documents/code/", status_interval = update_interval, use_mscale = use_muscale,
                               use_bscale = use_tauscale, b_half_normal = TRUE)

  m_post = muy + sdy * fitbcf$m_post[, order(perm)]
  tau_post = sdy * fitbcf$b_post[, order(perm)]
  list(sigma = sdy * fitbcf$sigma,
       yhat = muy + sdy * fitbcf$yhat_post[, order(perm)],
       tau = tau_post,
       mu_scale = fitbcf$msd * sdy,
       tau_scale = fitbcf$bsd * sdy,
       perm = perm,
       treatment = sdy * fitbcf$b_post[, order(perm)]
       )
}
