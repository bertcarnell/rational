#' @include rational-class.R
NULL

.printRational <- function(n, d, v)
{
  print(paste0("(", n, " / ", d, ") = ", v))
}

.catRational <- function(n, d, v)
{
  cat(paste0("(", n, " / ", d, ") = ", v))
}

#' Print an object of class Rational
#'
#' @name rational-print
#' @param object the object to show
#' @param x rational object
#' @param ... other arguments passed to print
#'
#' @examples
#' a <- rational(1L, 3L, "S3")
#' print(a)
NULL

#' @rdname rational-print
#' @export
setMethod("print", signature = "rationalS4",
          function(x)
          {
            .printRational(x@n, x@d, x@v)
          })

#' @rdname rational-print
#' @export
setMethod("show", signature = "rationalS4",
          function(object)
          {
            .catRational(object@n, object@d, object@v)
          })

#' @rdname rational-print
#' @method print rationalS3
#' @export
print.rationalS3 <- function(x, ...)
{
  .printRational(x$n, x$d, x$v)
}

#' @rdname rational-print
#' @method print rationalR6
#' @export
print.rationalR6 <- function(x, ...)
{
  .printRational(x$getNumerator(), x$getDenominator(), x$getValue())
}
