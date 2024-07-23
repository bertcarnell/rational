#' @include rational-class.R
NULL

#' Base Print Method
#'
#' @param n numerator
#' @param d demoninator
#' @param v numeric value
#' @noRd
.printRational <- function(n, d, v)
{
  print(paste0("(", n, " / ", d, ") = ", v))
}

#' Base cat Method
#'
#' @param n numerator
#' @param d demoninator
#' @param v numeric value
#' @noRd
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
#' @importFrom methods setMethod
#' @importFrom S7 new_generic S7_dispatch method
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

#' @rdname rational-print
#' @method print rationalS7
#' @export
# Similar to print.S7_object in https://github.com/RConsortium/S7/blob/main/R/class.R
print.rationalS7 <- function(x, ...)
{
  .printRational(x@n, x@d, x@v)
}
