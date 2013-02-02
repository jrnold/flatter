##' @exportClass FlattenedArray
NULL

##' Flattened Array
##'
##' This class stores an array as a vector of data, and a matrix of
##' arrays.
##'
##' @section Slots:
##'
##' \describe{
##' \item{x}{A vector of containing the values.}
##' \item{indices}{An integer matrix with the indices associated with each element in the \code{.Data} vector}
##' \item{dim}{Dimenensions of the array}
##' \item{default}{Value to use to fill in any array indices not specified in \code{indices}.}
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{vector}{Directly}
##' }
##' @name FlattenedArray-class
##' @aliases FlattenedArray-class
NULL
FlattenedArray <-
    setClass("FlattenedArray",
             representation=list(x="vector", indices="matrix",
             dim="integer", default="ANY"))

flattened_array_validity <- function(object) {
    if (length(object@x) != nrow(object@indices)) {
        return("length of object@x != number of rows in object@indices")
    }
    if (length(object@dim) != ncol(object@indices)) {
        return("length of object@dim != number of columns in object@indices")
    }
    # Only check if object@x has any entries
    if (length(object@x)) {
        maxind <- apply(object@indices, 2, max)
        if (any(maxind > object@dim)) {
            return("Invalid indices. All indices must be <= dim.")
        }
        minind <- apply(object@indices, 2, min)
        if (any(maxind < 0)) {
            return("Invalid indices. All indices must be >= 0.")
        }
    }
    TRUE
}

setValidity("FlattenedArray", flattened_array_validity)

flattened_array_initialize <- function(.Object, x, indices=seq_along(x),
                                       dim=apply(indices, 2, max), default=NA)
{
    .Object@x <- as.vector(x)
    .Object@indices <- as.matrix(indices)
    .Object@dim <- as.integer(dim)
    .Object@default <- default
    validObject(.Object)
    .Object
}

setMethod("initialize", "FlattenedArray", flattened_array_initialize)
