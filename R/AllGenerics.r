##' @exportMethod flatten
##' @exportMethod unflatten
NULL

##' Flatten matrices, arrays, and vectors.
##'
##' @param x Object to flatten; a vector, matrix, or array.
##' @param default Value of \code{x} which are the default value of the array.
##' @param drop_default Drop indices which have the default value.
##' @return Object of class \code{FlattenedArray}.
##' @param ... passed to next method.
##'
##' @name flatten-methods
##' @aliases flatten
##' @export
setGeneric("flatten", function(x, ...) standardGeneric("flatten"))

##' Unflatten array
##'
##' Convert a flattened array back to an array.
##'
##' @param x Object with the elements in the array
##' @param i Indices of the objects. Not necessary if \code{x} is a
##' \code{FlattenedArray}.
##' @param default Value used to fill any elements in the array not specified in \code{x} and \code{i}.
##' @param dim Dimensions of the array
##'
##' @return Object of class \code{FlattenedArray}.
##'
##' @name unflatten-methods
##' @aliases unflatten
##' @export
setGeneric("unflatten", function(x, i, ...) standardGeneric("unflatten"))

