##' Flatten constraint matrix
##'
##' Given a matrix (described below), this will create a list of
##' vectors which give the locations of unconstrained, constrant,
##' positive, and negative elements.
##'
##' @param x Numeric \code{matrix} with \code{NA} for unconstrained
##' elements, a finite value for constrant entries, \code{Inf} for
##' elements to be bound below at 0, and \code{-Inf} for elements to
##' be bound above at 0.
##'
##' @return \code{list} with elements
##' \describe{
##' \item{\code{var_n}}{Number of unstrained entries}
##' \item{\code{var_i}}{Indices of unconstrained entries}
##' \item{\code{const_n}}{Number of constant entries}
##' \item{\code{const_val}}{Values of constant entries}
##' \item{\code{const_i}}{Indices of constant entries}
##' \item{\code{pos_n}}{Number of positive entries (truncated below at 0)}
##' \item{\code{pos_i}}{Indices of positive entries}
##' \item{\code{pos_n}}{Number of negative entries (truncated above at 0)}
##' }
##'
##' @examples
##' constraints <- matrix(NA, 4, 2)
##' # constants
##' constraints[1, 1] <- 1
##' constraints[1, 2] <- 0
##' # positive values
##' constraints[2, 1] <- Inf
##' # negative values
##' constraints[2, 2] <- -Inf
##' flatten_constraints(constraints)
##'
##' @export
flatten_constraints <- function(x) {
    flatx <- flatten(x, drop_default=FALSE)
    is_var <- is.na(x)
    is_pos <- !is.na(x) & x == Inf
    is_neg <- !is.na(x) & x == -Inf
    is_const <- ! (is.na(x) | is.infinite(x))
    list(## unconstrained
         var_n = sum(is_var),
         var_i = flatx@indices[is_var, , drop=FALSE],
         ## Constants
         const_n = sum(is_const),
         const_val = flatx@x[is_const],
         const_i = flatx@indices[is_const, , drop=FALSE],
         ## positive
         pos_n = sum(is_pos),
         pos_i = flatx@indices[is_pos, , drop=FALSE],
         ## negative
         neg_n = sum(is_neg),
         neg_i = flatx@indices[is_neg, , drop=FALSE]
         )
}


##' Lower Triangular Factor Constraints
##'
##' Create lower-triangular constraints as used for the loadings matrix in
##' confirmatory factor analysis.
##'
##' @param n Number of variables
##' @param factors Number of factors
##' @param ones If \code{TRUE}, then the diagonal is equal to 1, if
##' \code{FALSE} the diagonal is set to \code{Inf} (positive).
##'
##' @return Matrix with \code{n} rows and \code{factors} columns, with
##' 1's or \code{Inf} on the diagonal, \code{NA} below the diagonal, and
##' 0's above the diagonal.
##' @seealso \code{\link{flatten_constraints}}
##'
##' @examples
##' constraints <- lowertri_constraint_matrix(3, 2)
##' constraints
##' flatten_constraints(constraints)
##' 
##' @export
lowertri_constraint_matrix <- function(n, factors=1, ones=TRUE) {
    y <- matrix(NA_real_, n, factors)
    if (ones == TRUE) {
        diag(y) <- 1
    } else {
        diag(y) <- Inf
    }
    y[row(y) < col(y)] <- 0
    y
}

##' Given a vector, return the appropriate NA type
##'
##' @param x Object
##' @return A one-element vector containing the \code{NA} constant associated
##' with the class of \code{x}.
##'
##' @seealso \code{\link{NA}} for a description of different missing
##' value constants.
##' 
##' @keywords internal
na_type <- function(x) {
    if (is.character(x)) {
        NA_character_
    } else if (is.integer(x)) {
        NA_integer_
    } else if (is.complex(x)) {
        NA_complex_
    } else if (is.numeric(x)) {
        NA_real_
    } else {
        NA
    }
}

