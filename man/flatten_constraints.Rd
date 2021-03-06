% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.r
\name{flatten_constraints}
\alias{flatten_constraints}
\title{Flatten constraint matrix}
\usage{
flatten_constraints(x)
}
\arguments{
\item{x}{Numeric \code{matrix} with \code{NA} for unconstrained
elements, a finite value for constrant entries, \code{Inf} for
elements to be bound below at 0, and \code{-Inf} for elements to
be bound above at 0.}
}
\value{
\code{list} with elements
\describe{
\item{\code{var_n}}{Number of unstrained entries}
\item{\code{var_i}}{Indices of unconstrained entries}
\item{\code{const_n}}{Number of constant entries}
\item{\code{const_val}}{Values of constant entries}
\item{\code{const_i}}{Indices of constant entries}
\item{\code{pos_n}}{Number of positive entries (truncated below at 0)}
\item{\code{pos_i}}{Indices of positive entries}
\item{\code{pos_n}}{Number of negative entries (truncated above at 0)}
}
}
\description{
Given a matrix (described below), this will create a list of
vectors which give the locations of unconstrained, constrant,
positive, and negative elements.
}
\examples{
constraints <- matrix(NA, 4, 2)
# constants
constraints[1, 1] <- 1
constraints[1, 2] <- 0
# positive values
constraints[2, 1] <- Inf
# negative values
constraints[2, 2] <- -Inf
flatten_constraints(constraints)
}

