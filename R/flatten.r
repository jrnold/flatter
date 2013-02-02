## Variation of reshape2:::melt.array
flatten_array <- function(x, default=NA, drop_default=TRUE) {
    default <- default[1]
    dimensions <- dim(x)
    dnm <- sapply(dimensions, seq_len)
    i <- as.matrix(expand.grid(dnm))
    x <- as.vector(x)
    if (drop_default) {
        if (is.na(default)) {
            is_default <- is.na(x)
        } else {
            is_default <- x == default
        }
        x <- x[!is_default]
        i <- i[!is_default, ]
    }
    FlattenedArray(x, i, dim=dimensions, default=default)
}

##' @rdname flatten-methods
##' @aliases flatten,array-method
setMethod("flatten", "array", flatten_array)

flatten_vector <- function(x, ... ) {
    flatten(as.array(x))
}

##' @rdname flatten-methods
##' @aliases flatten,vector-method
setMethod("flatten", "vector", flatten_vector)

