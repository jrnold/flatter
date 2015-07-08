##' @exportMethod [[
##' @exportMethod [
NULL

setMethod("[[", c(x="FlattenedArray", i="integer", j="missing"),
          function(x, i, j, ...) {
              x@x[i]
          })

setMethod("[", c(x="FlattenedArray"),
          function(x, i, j, ..., drop=TRUE) {
              unflatten(x)[i, j, ..., drop=TRUE]
          })
