% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy-posterior-bartMachine.R
\name{predicted_draws.bartMachine}
\alias{predicted_draws.bartMachine}
\title{Get predict draws from posterior of \code{bartMachine} model}
\usage{
\method{predicted_draws}{bartMachine}(
  object,
  newdata,
  value = ".prediction",
  ...,
  ndraws = NULL,
  include_newdata = TRUE,
  include_fitted = FALSE,
  include_sigsqs = FALSE
)
}
\arguments{
\item{object}{A \code{bartMachine} model.}

\item{newdata}{Data frame to generate predictions from. If omitted, most model types will generate predictions from the data used to fit the model.}

\item{value}{The name of the output column for \code{predicted_draws}; default \code{".prediction"}.}

\item{...}{Not currently in use.}

\item{ndraws}{Not currently implemented.}

\item{include_newdata}{Should the newdata be included in the tibble?}

\item{include_fitted}{Should the posterior fitted values be included in the tibble?}

\item{include_sigsqs}{Should the posterior sigma-squared draw be included?}
}
\value{
A tidy data frame (tibble) with predicted values.
}
\description{
Get predict draws from posterior of \code{bartMachine} model
}
