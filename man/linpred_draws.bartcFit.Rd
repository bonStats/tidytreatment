% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy-posterior-bartCause.R
\name{linpred_draws.bartcFit}
\alias{linpred_draws.bartcFit}
\title{Get expected prediction draws (on linear scale) from posterior of \code{bartCause}-package objects}
\usage{
\method{linpred_draws}{bartcFit}(
  object,
  ...,
  value = ".linpred",
  re_formula = NULL,
  fitstage = c("response", "assignment")
)
}
\arguments{
\item{object}{A \code{bartCauseFit} object.}

\item{...}{Additional arguments (e.g. \code{newdata}) passed to the underlying prediction method for the type of model given.}

\item{value}{The name of the output column.}

\item{re_formula}{If NULL (default), include all group-level effects; if NA, include no group-level effects.}

\item{fitstage}{If \code{is.null(type)}, return posterior from \code{response} or treatment \code{assignment} model.}
}
\description{
Typically referred to as fitted value draws on linear scale, where appropriate.
}
