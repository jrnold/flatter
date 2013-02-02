##' @exportMethod [[
##' @exportMethod [
NULL

##' Extract methods for \code{FlattenedArray} objects
##'
##' Extract methods for objects of class \code{FlattenedArray}.
##' 
##' Right now they only extract methods are implemented, no insertion
##' methods. These are not particularly efficient either.
##'
##' @name extract-method
##' @aliases [,FlattenedArray-method
##' @aliases [[,FlattenedArray,integer,missing-method
NULL
setMethod("[[", c(x="FlattenedArray", i="integer", j="missing"),
          function(x, i, j, ...) {
              x@x[i]
          })

setMethod("[", c(x="FlattenedArray"),
          function(x, i, j, ..., drop=TRUE) {
              unflatten(x)[i, j, ..., drop=TRUE]
          })
