% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common-support.R
\name{has_common_support}
\alias{has_common_support}
\title{Evaluate if observations have common support.}
\usage{
has_common_support(model, treatment, method, cutoff, modeldata = NULL)
}
\arguments{
\item{model}{A supported Bayesian model fit that can provide fits and predictions.}

\item{treatment}{A character string specifying the name of the treatment variable.}

\item{method}{Method to use in determining common support. 'chisq', or 'sd'.}

\item{cutoff}{Cutoff point to use for method.}

\item{modeldata}{Manually provide model data for some models (e.g. from BART package)}
}
\value{
Tibble with a row for each observation and a column indicating whether common support exists.
}
\description{
The common support identification methods are based on Hill and Su (2013).
Loosely speaker, an individuals treatment effect estimate has common support if the counter factual
estimate is not too uncertain. The estimates are uncertain when the prediction is 'far away' from
other observations. Removing estimates without common support can be beneficial for treat effect
estimates.
}
\details{
Hill, Jennifer; Su, Yu-Sung. Ann. Appl. Stat. 7 (2013), no. 3, 1386--1420. doi:10.1214/13-AOAS630. \url{https://projecteuclid.org/euclid.aoas/1380804800}
}
