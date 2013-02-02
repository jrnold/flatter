\name{FlattenedArray-class}
\alias{FlattenedArray-class}
\title{Flattened Array}
\description{
  This class stores an array as a vector of data, and a
  matrix of arrays.
}
\section{Slots}{
  \describe{ \item{x}{A vector of containing the values.}
  \item{indices}{An integer matrix with the indices
  associated with each element in the \code{.Data} vector}
  \item{dim}{Dimenensions of the array}
  \item{default}{Value to use to fill in any array indices
  not specified in \code{indices}.} }
}

\section{Extends}{
  \describe{ \item{vector}{Directly} }
}

