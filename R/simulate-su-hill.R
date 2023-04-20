#' Simulate data with scenarios from Hill and Su (2013)
#'
#' Sample \eqn{n} observations with the following scheme:
#' \enumerate{
#'   \item Covariates: \eqn{X_j ~ N(0,1)}.
#'   \item Assignment: \eqn{Z ~ Bin(n, p)}  with \eqn{p = logit^{-1}(a + X \gamma^L + Q \gamma^N)} where \eqn{a = \omega - mean(X \gamma^L + Q \gamma^N)}.
#'   \item Mean response: \eqn{E(Y(0)|X) = X \beta_0^L + Q \beta_0^N } and \eqn{E(Y(1)|X) = X \beta_1^L + Q \beta_1^N}.
#'   \item Observation: \eqn{Y ~ N(\mu,\sigma_y^2))}.
#' }
#' Superscript \eqn{L} denotes the linear components, whilst \eqn{N} denotes the non-linear
#' components.
#'
#' Coefficients used are returned in the list this function creates. See Table 1 in Su and Hill (2013) for the table of coefficients.
#' The \eqn{X_j} are in a data.frame named \code{data} in the returned list.
#' The formula for the model matrix \eqn{[X,Q]} is named \code{su_hill_formula} in the returned list.
#' The coefficients used for the model matrix are contained in \code{coefs}.
#' The Su and Hill (2013) simulations did not include categorical variables, but you can add them here using arguments: \code{add_categorical}, \code{coef_categorical_treatment}, \code{coef_categorical_nontreatment}.
#'
#' Hill, Jennifer; Su, Yu-Sung. Ann. Appl. Stat. 7 (2013), no. 3, 1386--1420. doi:10.1214/13-AOAS630. \url{https://projecteuclid.org/euclid.aoas/1380804800}
#'
#' @param n Size of simulated sample.
#' @param tau Treatment effect for parallel response surfaces. Not applicable if surface is nonparallel.
#' @param omega Offset to control treatment assignment ratios.
#' @param treatment_linear Treatment assignment mechanism is linear?
#' @param response_parallel Response surface is parallel?
#' @param response_aligned Response surface is aligned?
#' @param y_sd Observation noise.
#' @param add_categorical Should a categorical variable be added? (Not in Hill and Su)
#' @param n_subjects How many subjects are there? For repeated observations. (Hill and Su = 0, default)
#' @param sd_subjects Random effect intercept standard deviation for subjects. (Not in Hill and Su. Used if n_subjects > 0)
#' @param coef_categorical_treatment What are the coefficients of the categorical variable under treatment? (Not in Hill and Su)
#' @param coef_categorical_nontreatment What are the coefficients of the categorical variable under nontreatment? (Not in Hill and Su)
#' @return An object of class \code{suhillsim} that is a list with elements
#' \item{data}{Simulated data in data.frame}
#' \item{mean_y}{The mean y values for each individual (row)}
#' \item{args}{List of arguments passed to function}
#' \item{formulas}{Response formulas used to generate data}
#' \item{coefs}{Coefficients for the formulas}
#' @export
simulate_su_hill_data <- function(n, treatment_linear = TRUE, response_parallel = TRUE, response_aligned = TRUE, y_sd = 1, tau = 4, omega = 0, add_categorical = FALSE, n_subjects = 0, sd_subjects = 1, coef_categorical_treatment = NULL, coef_categorical_nontreatment = NULL) {
  fargs <- as.list(match.call())

  coefs <- dplyr::tribble(
    ~"class", ~"linear", ~"parallel", ~"aligned", ~"treatment", ~"values",
    # treatment assignment: linear, nonlinear
    "treatment-assignment", TRUE, NA, NA, NA, c(0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.2, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "treatment-assignment", FALSE, NA, NA, NA, c(0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.2, 0.4, 0.2, 0.4, 0.2, 0.8, 0.8, 0.5, 0.3, 0.8, 0.2, 0.4, 0.3, 0.8, 0.5),
    # response surface nonlinear and not parallel, aligned: treatment, nontreatment
    "response", FALSE, FALSE, TRUE, FALSE, c(0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 2.0, 0.0, 0.5, 2.0, 0.4, 0.8, 0.0, 0.0, 0.5, 0.0, 0.5, 0.0, 0.5, 0.7),
    "response", FALSE, FALSE, TRUE, TRUE, c(0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.0, 1.0, 0.5, 0.0, 0.8, 0.0, 0.0, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    # response surface nonlinear and not parallel, not as aligned: treatment, nontreatment
    "response", FALSE, FALSE, FALSE, FALSE, c(0.5, 2.0, 0.4, 0.5, 1.0, 0.5, 2.0, 0.0, 0.0, 0.0, 0.0, 0.5, 1.5, 0.7, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    "response", FALSE, FALSE, FALSE, TRUE, c(0.5, 0.5, 0.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  )

  # add parallel response surfaces:
  add_parallel_nontreatment <-
    dplyr::mutate(
      dplyr::filter(coefs, class == "response", !.data$treatment),
      parallel = TRUE
    )

  add_parallel_treatment <- dplyr::mutate(
    add_parallel_nontreatment,
    treatment = TRUE
  )

  coefs <- dplyr::bind_rows(coefs, add_parallel_nontreatment, add_parallel_treatment)

  coefs_treatment_assignment <-
    dplyr::filter(
      coefs,
      class == "treatment-assignment",
      .data$linear == treatment_linear
    )

  coefs_response <-
    dplyr::filter(
      coefs,
      class == "response",
      .data$parallel == response_parallel,
      .data$aligned == response_aligned
    )

  stopifnot(
    nrow(coefs_treatment_assignment) == 1,
    nrow(coefs_response) == 2
  )

  coef_assign <- coefs_treatment_assignment$values[[1]]
  coef_y_0 <- dplyr::filter(coefs_response, .data$treatment == FALSE)$values[[1]]
  coef_y_1 <- dplyr::filter(coefs_response, .data$treatment == TRUE)$values[[1]]

  invlogit <- function(x) {
    exp(x) / (1 + exp(x))
  }

  # simulate data
  X <- as.data.frame(matrix(rnorm(n * 10, mean = 0, sd = 1), ncol = 10))
  colnames(X) <- paste0("x", 1:10)

  su_hill_formula <-
    ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x2 * x6) +
      x5 + x6 + x7 + x8 + x9 + x10 +
      I(x5^2) + I(x6^2) + I(x5 * x6) + I(x5 * x6 * x7) + I(x7^2) +
      I(x7^3) + I(x8^2) + I(x7 * x8) + I(x9^2) + I(x9 * x10)

  model_matrix <- as.matrix(stats::model.frame(su_hill_formula, data = X))


  # assign to treatment
  logit_mean_treat_assignment <- model_matrix %*% coef_assign

  allocation_offset <- omega - mean(logit_mean_treat_assignment)

  p <- invlogit(allocation_offset + logit_mean_treat_assignment)
  z <- stats::rbinom(n = n, size = 1, prob = p)

  # Add categorical variable. Note: not included in Hill and Su

  if (add_categorical) {
    stopifnot(
      is.numeric(coef_categorical_treatment),
      is.numeric(coef_categorical_nontreatment),
      length(coef_categorical_treatment) ==
        length(coef_categorical_nontreatment)
    )

    ss <- length(coef_categorical_treatment)

    # sample categories with equal probability
    c1 <- sample.int(n = ss, replace = TRUE, size = n)

    cat_y_0 <- coef_categorical_nontreatment[c1]
    cat_y_1 <- coef_categorical_treatment[c1]
  } else {
    cat_y_0 <- 0
    cat_y_1 <- 0
  }

  # mean response
  mean_y <- ifelse(z == 0, model_matrix %*% coef_y_0 + cat_y_0, model_matrix %*% coef_y_1 + response_parallel * tau + cat_y_1)

  # add noise
  y <- rnorm(n = n, mean = mean_y, sd = y_sd)

  if (add_categorical) {
    rdata <- cbind(data.frame(y = y, z = z, c1 = factor(c1)), X)
  } else {
    rdata <- cbind(data.frame(y = y, z = z), X)
  }

  if (n_subjects > 0) {
    # add subject effects
    rdata <- rdata %>% mutate(subject_id = factor(sample(1:n_subjects, nrow(rdata), replace = T)))
    subject_effect <- rnorm(n_subjects, sd = sd_subjects)
    rdata$y <- rdata$y + subject_effect[rdata$subject_id]
  }

  # prepare formula's to describe simulation truth
  formula_terms <- attributes(terms(su_hill_formula))$term.labels

  frmls <- list()

  # formula for treatment assignment
  which_treatment_assignment <- coefs_treatment_assignment$values[[1]]
  chr_treatment_assignment_frm <- paste(paste0(which_treatment_assignment, "*", formula_terms)[which_treatment_assignment != 0], collapse = " + ")
  frmls$treatment_assignment <- parse(text = chr_treatment_assignment_frm)

  # formula for response from treatment group
  which_response_treatment <- dplyr::filter(coefs_response, .data$treatment == TRUE)$values[[1]]
  chr_response_treatment_frm <- paste(paste0(which_response_treatment, "*", formula_terms)[which_response_treatment != 0], collapse = " + ")
  if (add_categorical) {
    add_chr_response_treatment_frm_cat <- paste0(coef_categorical_treatment, "*I(c1==", paste0("'", 1:length(coef_categorical_treatment), "'"), ")")[coef_categorical_treatment != 0]
    chr_response_treatment_frm <- paste(chr_response_treatment_frm, "+", paste(add_chr_response_treatment_frm_cat, collapse = " + "))
  }
  frmls$response_treatment <- parse(text = chr_response_treatment_frm)

  # formula for response from non-treatment group
  which_response_nontreatment <- dplyr::filter(coefs_response, .data$treatment == FALSE)$values[[1]]
  chr_response_nontreatment_frm <- paste(paste0(which_response_nontreatment, "*", formula_terms)[which_response_nontreatment != 0], collapse = " + ")
  if (add_categorical) {
    add_chr_response_nontreatment_frm_cat <- paste0(coef_categorical_nontreatment, "*I(c1==", paste0("'", 1:length(coef_categorical_nontreatment), "'"), ")")[coef_categorical_nontreatment != 0]
    chr_response_nontreatment_frm <- paste(chr_response_nontreatment_frm, "+", paste(add_chr_response_nontreatment_frm_cat, collapse = " + "))
  }
  frmls$response_nontreatment <- parse(text = chr_response_nontreatment_frm)

  frmls$generic <- su_hill_formula


  return(
    structure(list(
      data = rdata,
      mean_y = mean_y,
      args = fargs,
      formulas = frmls,
      coefs = dplyr::bind_rows(coefs_treatment_assignment, coefs_response)
    ), class = "suhillsim")
  )
}

#' @export
print.suhillsim <- function(x, ...) {
  st <- "Su-Hill Simulation"
  ta <- paste0("\n  Treatment assignment: ", as.character(x$formulas$treatment_assignment))
  rt <- paste0("\n  Response (treated): ", as.character(x$formulas$response_treatment))
  rnt <- paste0("\n  Response (nontreated): ", as.character(x$formulas$response_nontreatment))
  en <- paste0("\nList with elements\n", paste0("\t$", names(x), collapse = "\n"))

  cat(st, ta, rt, rnt, en)
}
