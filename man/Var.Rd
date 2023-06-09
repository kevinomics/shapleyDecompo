% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inequality_w.R
\name{Var}
\alias{Var}
\title{Compute Var measure of inequality}
\usage{
Var(distri, weights)
}
\arguments{
\item{distri}{Distribution of outcomes.}

\item{weights}{Weights used in the econometric model.}
}
\value{
A value of inequality.
}
\description{
Compute Var measure of inequality
}
\examples{
distri <- rnorm(mean = 20, n = 100)
weights <- rep(1, 100)
Var(distri, weights)
}
