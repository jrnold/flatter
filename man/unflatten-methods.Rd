\name{unflatten-methods}
\alias{unflatten}
\alias{unflatten,FlattenedArray,missing-method}
\alias{unflatten-methods}
\alias{unflatten,vector,matrix-method}
\alias{unflatten,vector,vector-method}
\title{Unflatten array}
\arguments{
  \item{x}{Object with the elements in the array}

  \item{i}{Indices of the objects. Not necessary if
  \code{x} is a \code{FlattenedArray}.}

  \item{default}{Value used to fill any elements in the
  array not specified in \code{x} and \code{i}.}

  \item{dim}{Dimensions of the array}
}
\value{
  Object of class \code{FlattenedArray}.
}
\description{
  Convert a flattened array back to an array.
}

