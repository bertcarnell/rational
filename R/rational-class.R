#' @title Rational Number Classes
#'
#' @description An S3, S4, and R6 class for a rational number
#' \itemize{
#'    \item rationalS3 - access fields with the '$' operator.
#'    \item rationalS4 - access fields with the '@@' operator.
#'    \item rationalR6 - internal elements are private, so fields are accessed with accessor methods,
#'      \code{$getNumerator(), $getDenominator(), $getValue()}
#' }
#' The classes are designed to be used in a similar way to integers and numerics in R.
#'
#' @param n the numerator
#' @param d the denominator
#' @field n,d,v the numerator, denominator, and value field of the S3 class
#' @slot n the numerator of the S4 class
#' @slot d the denominator of the S4 class
#' @slot v the \code{numeric} value of the S4 class
#' @name rational-class
#'
#' @importFrom methods callGeneric callNextMethod is new show
#' @importFrom R6 R6Class
#' @importFrom S7 new_class new_object S7_object
NULL

# error messages
.rationalError0 <- "rational class error:"
.rationalErrorMessage1 <- paste(.rationalError0, "the numerator and denominator must be integers",
                                "\nPlease specify using as.integer([number]) or [number]L",
                                "\ne.g. as.integer(7), as.integer(c(1,2,3)), 3L, c(1L,2L,3L)",
                                "\nNA, NaN, Inf, NULL are not permitted")
.rationalErrorMessage2 <- paste(.rationalError0, "The denominator of the rational number must be non-zero")
.rationalErrorMessage3 <- paste(.rationalError0, "The numerator and denominator must have equal length")
.rationalErrorMessage4 <- paste(.rationalError0, "The method argument may only be of length 1")
.rationalErrorMessage5 <- paste(.rationalError0, "the method must be one of S3, S4, R6, or S7")
.rationalErrorMessage6 <- paste(.rationalError0, "binary operators of rational numbers require that the numbers be the same length")
.rationalErrorMessage7 <- paste(.rationalError0, "only rationalR6 objects can be insterted into a rationalR6 vector")
.rationalErrorMessage8 <- paste(.rationalError0, "only rationalS3 objects can be insterted into a rationalS3 vector")

#' rational number S3 class generator
#'
#' @noRd
#' @author Rob Carnell
#' @param n integer numerator
#' @param d integer denominator (non-zero)
.rationalS3 <- function(n, d)
{
  # for consistency, integrity checks are done in the generating function
  ret <- list(n = n, d = d, v = n / d)
  class(ret) <- "rationalS3"
  return(ret)
}

#' rational number R6 class generator
#'
#' for consistency, integrity checks are done in the generating function
#'
#' @noRd
#' @author Rob Carnell
#' @field n integer numerator
#' @field d integer denominator
#' @field v numeric value
#' @field initialize initialization function
.rationalR6 <- R6Class("rationalR6",
                       public = list(
                         getNumerator = function() private$n,
                         getDenominator = function() private$d,
                         getValue = function() private$v,
                         initialize = function(n, d)
                         {
                           private$n <- n
                           private$d <- d
                           private$v <- n / d
                           self
                         },
                         setNumerator = function(x)
                         {
                           private$n <- x
                           private$v <- private$n / private$d
                         },
                         setDenominator = function(x)
                         {
                           private$d <- x
                           private$v <- private$n / private$d
                         },
                         assign_at = function(i, value)
                         {
                           private$n[i] <- value$getNumerator()
                           private$d[i] <- value$getDenominator()
                           private$v <- private$n / private$d
                         }),
                       private = list(
                         n = 1L,
                         d = 1L,
                         v = 1L
                       ), lock_class = FALSE, lock_objects = TRUE, portable = TRUE)

#' rational number S4 class generator
#'
#' @noRd
#'
#' @slot n integer numerator
#' @slot d integer denominator
#' @slot v numeric value
setClass("rationalS4", slots = c(n = "integer", d = "integer", v = "numeric"),
         valid = function(object)
         {
           if (length(object@n) == length(object@d)) {
             if (all(is.integer(object@n)) && all(is.integer(object@d))) {
               if (!any(object@d == 0)) return(TRUE)
               else return(.rationalErrorMessage2)
             }
             else return(.rationalErrorMessage1)
           }
           else return(.rationalErrorMessage3)
         })

#' initialize method for the \code{rationalS4} class
#'
#' @noRd
#'
#' @param .Object the instance of the class
#' @return the initialized class object
setMethod("initialize", "rationalS4", function(.Object, n, d)
{
  .Object@n <- n
  .Object@d <- d
  .Object@v <- n / d
  # validity checks happen on the default initialize which is overridden here
  #  so call default initialize with callNextMethod
  callNextMethod(.Object = .Object, n = n, d = d)
})

#' Rational number S7 class
#'
#' @noRd
#'
#' @author Rob Carnell
#' @slot n integer numerator
#' @slot d integer denominator
#' @slot v numeric value
rationalS7 <- S7::new_class("rationalS7",
                             properties = list(
                               n = S7::class_integer,
                               d = S7::class_integer,
                               v = S7::class_numeric
                             ),
                             validator = function(self) {
                               if (length(self@n) != length(self@d)) {
                                 return("@n and @d must be the same length")
                               }
                               if (any(is.na(self@n)) | any(is.na(self@d))) {
                                 return("@n and @d must not be NA or NaN")
                               }
                             },
                             constructor = function(.data, n, d) {
                               stopifnot(is.integer(n), is.integer(d))
                               stopifnot(all(d != 0L))
                               S7::new_object(S7::S7_object, n = n, d = d, v = n / d)
                             },
                             parent = S7::S7_object,
                             package = "rational"
)


#' rational number generator for all classes
#'
#' generator rational number of class \code{rationalS3}, \code{rationalS4},
#' and \code{rationalR6}.  Each type of class has advantages and disadvantages
#' in performance and flexibility
#'
#' @note note that \code{Inf}, \code{NA}, \code{NaN}, and \code{NULL} all fail
#' on is.integer() and are not permitted
#'
#' @export
#' @param method a length = 1 character vector.  One of "R6" (default), "S3", "S4"
#' @return the desired instance of the rational class
#' @rdname rational-class
#' @examples
#'  a <- rational(1L, 3L, method="S3")
#'  stopifnot(a$n == 1L && a$d == 3L && abs(a$v - 1/3) < 1E-12)
#'  stopifnot(class(a) == "rationalS3")
#'
#'  b <- rational(2L, 5L, method="S4")
#'  stopifnot(b@@n == 2L && b@@d == 5L && abs(b@@v - 2/5) < 1E-12)
#'  stopifnot(class(b) == "rationalS4" && isS4(b) && is(b, "rationalS4"))
#'
#'  d <- rational(3L, 7L, method="R6")
#'  stopifnot(d$getNumerator() == 3L && d$getDenominator() == 7L)
#'  stopifnot(abs(d$getValue() - 3/7) < 1E-12)
#'  stopifnot(class(d)[1] == "rationalR6" && is(d, "rationalR6") && is(d, "R6"))
#'
#'  e <- rational(3L, 7L, method="S7")
#'  stopifnot(e@@n == 3L && e@@d == 7L && abs(e@@v - 3/7) < 1E-12)
#'  stopifnot(class(e)[1] == "rational::rationalS7")
#'  stopifnot(is(e, "rational::rationalS7") && is(e, "S7_object"))
rational <- function(n, d, method="R6")
{
  if (!all(is.integer(n)) || !all(is.integer(d)))
    stop(.rationalErrorMessage1)
  if (any(d == 0))
    stop(.rationalErrorMessage2)
  if (length(n) != length(d))
    stop(.rationalErrorMessage3)
  if (length(method) != 1)
    stop(.rationalErrorMessage4)
  if (method == "R6")
  {
   return(.rationalR6$new(n, d))
  } else if (method == "S3")
  {
   return(.rationalS3(n, d))
  } else if (method == "S4")
  {
   return(new("rationalS4", n = n, d = d))
  } else if (method == "S7")
  {
    return(rationalS7(n = n, d = d))
  } else
  {
   stop(.rationalErrorMessage5)
  }
}
