% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/AllGenerics.r, R/unflatten.r
\docType{methods}
\name{unflatten-methods}
\alias{unflatten}
\alias{unflatten,FlattenedArray,missing-method}
\alias{unflatten,vector,matrix-method}
\alias{unflatten,vector,vector-method}
\alias{unflatten-methods}
\title{Unflatten array}
\usage{
unflatten(x, i, ...)

\S4method{unflatten}{vector,matrix}(x, i, default = na_type(x), dim = NULL)

\S4method{unflatten}{vector,vector}(x, i, ...)

\S4method{unflatten}{FlattenedArray,missing}(x, i)
}
\arguments{
\item{x}{Object with the elements in the array}

\item{i}{Indices of the objects. Not necessary if \code{x} is a
\code{FlattenedArray}.}

\item{...}{Not used.}

\item{default}{Value used to fill any elements in the array not specified in \code{x} and \code{i}.}

\item{dim}{Dimensions of the array}
}
\value{
Object of class \code{FlattenedArray}.

An array object
}
\description{
Convert a flattened array back to an array.

Expand a flattened object
}

