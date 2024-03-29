% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy-posterior-BART.R
\name{residual_draws.pbart}
\alias{residual_draws.pbart}
\title{Get residual draw for \code{pbart} model}
\usage{
\method{residual_draws}{pbart}(
  object,
  newdata,
  value = ".residual",
  ...,
  ndraws = NULL,
  include_newdata = TRUE,
  include_sigsqs = FALSE
)
}
\arguments{
\item{object}{\code{wbart} model.}

\item{newdata}{Data frame to generate predictions from. If omitted, original data used to fit the model.}

\item{value}{Name of the output column for residual_draws; default is \code{.residual}.}

\item{...}{Additional arguments passed to the underlying prediction method for the type of model given.}

\item{ndraws}{Not currently implemented.}

\item{include_newdata}{Should the newdata be included in the tibble?}

\item{include_sigsqs}{Should the posterior sigma-squared draw be included?}
}
\value{
Tibble with residuals.
}
\description{
The original response variable must be passed as an argument to this function.
e.g. `response = y`
}
