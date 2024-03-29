% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy-posterior-BART.R
\name{residual_draws_BART}
\alias{residual_draws_BART}
\title{Get residual draw for BART model}
\usage{
residual_draws_BART(
  object,
  response,
  newdata = NULL,
  value = ".residual",
  include_newdata = TRUE,
  include_sigsqs = FALSE
)
}
\arguments{
\item{object}{model from \code{BART} package.}

\item{response}{Original response vector.}

\item{newdata}{Data frame to generate predictions from. If omitted, original data used to fit the model.}

\item{value}{Name of the output column for residual_draws; default is \code{.residual}.}

\item{include_newdata}{Should the newdata be included in the tibble?}

\item{include_sigsqs}{Should the posterior sigma-squared draw be included?}
}
\value{
Tibble with residuals.
}
\description{
Classes from \code{BART}-package models
}
