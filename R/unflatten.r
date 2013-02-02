unflatten_default <- function(x, i, default=na_type(x), dim=NULL) {
    if (is.null(dim)) {
        dim <- apply(i, 2, max)
    }
    y <- array(default, dim)
    y[i] <- x
    y
}

##' @rdname unflatten-methods
##' @aliases unflatten,vector,matrix-method
setMethod("unflatten", c(x="vector", i="matrix"), unflatten_default)

##' @rdname unflatten-methods
##' @aliases unflatten,vector,vector-method
setMethod("unflatten", c(x="vector", i="vector"),
          function(x, i, ...) callGeneric(x, as.matrix(i), ...))

##' @rdname unflatten-methods
##' @aliases unflatten,FlattenedArray,missing-method
setMethod("unflatten", c(x="FlattenedArray", i="missing"),
          function(x, i) callGeneric(x@x, x@indices, default=x@default, dim=x@dim))


