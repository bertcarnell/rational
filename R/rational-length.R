#' @include rational-class.R
NULL

#' Rational class \code{length}
#'
#' @name rational-length
#' @param x An object of class \code{rational}
#' @param ... Other arguments passed to \code{length}
NULL

#' @rdname rational-length
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S4")
#'   stopifnot(length(a) == 3)
setMethod("length",
          "rationalS4",
          function(x)
          {
            callNextMethod(x@n)
          })

#' @rdname rational-length
#' @method length rationalS3
#' @export
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S3")
#'   stopifnot(length(a) == 3)
length.rationalS3 <- function(x, ...)
{
  return(length(x$n))
}

#' @rdname rational-length
#' @method length rationalR6
#' @export
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "R6")
#'   stopifnot(length(a) == 3)
length.rationalR6 <- function(x, ...)
{
  return(length(x$getNumerator()))
}
