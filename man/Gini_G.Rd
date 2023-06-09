% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inequality_w.R
\name{Gini_G}
\alias{Gini_G}
\title{Compute generalized Gini's measure of inequality}
\usage{
Gini_G(distri, theta, weights)
}
\arguments{
\item{distri}{Distribution of outcomes.}

\item{theta}{Value of sensibility.}

\item{weights}{Weights used in the econometric model.}
}
\value{
A value of inequality.
}
\description{
Compute generalized Gini's measure of inequality
}
\examples{
distri <- rnorm(mean = 20, n = 100)
weights <- rep(1, 100)
theta <- 1
Gini_G(distri, weights, theta)
}
