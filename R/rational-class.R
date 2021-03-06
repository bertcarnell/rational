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
.rationalErrorMessage5 <- paste(.rationalError0, "the method must be one of S3, S4, or R6")
.rationalErrorMessage6 <- paste(.rationalError0, "binary operators of rational numbers require that the numbers be the same length")
.rationalErrorMessage7 <- paste(.rationalError0, "only rationalR6 objects can be insterted into a rationalR6 vector")
.rationalErrorMessage8 <- paste(.rationalError0, "only rationalS3 objects can be insterted into a rationalS3 vector")

# rational number S3 class generator
#
# author Rob Carnell
# param n integer numerator
# param d integer denominator (non-zero)
# field n integer numerator
# field d integer denominator
# field v numeric value
.rationalS3 <- function(n, d)
{
  # for consistency, integrity checks are done in the generating function
  ret <- list(n = n, d = d, v = n / d)
  class(ret) <- "rationalS3"
  return(ret)
}

# rational number R6 class generator
#
# @author Rob Carnell
# @field n integer numerator
# @field d integer denominator
# @field v numeric value
# @field initialize initialization function
# @rdname rational-class
# for consistency, integrity checks are done in the generating function
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

# rational number S4 class generator
#
# slot n integer numerator
# slot d integer denominator
# slot v numeric value
# rdname rational-class
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

# initialize method for the \code{rationalS4} class
#
# param .Object the instance of the class
# return the initialized class object
# rdname rational-class
setMethod("initialize", "rationalS4", function(.Object, n, d)
{
  .Object@n <- n
  .Object@d <- d
  .Object@v <- n / d
  # validity checks happen on the default initialize which is overridden here
  #  so call default initialize with callNextMethod
  callNextMethod(.Object = .Object, n = n, d = d)
})

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
#'  b <- rational(2L, 5L, method="S4")
#'  stopifnot(b@@n == 2L && b@@d == 5L && abs(b@@v - 2/5) < 1E-12)
#'  stopifnot(class(b) == "rationalS4" && isS4(b) && is(b, "rationalS4"))
#'  d <- rational(3L, 7L, method="R6")
#'  stopifnot(d$getNumerator() == 3L && d$getDenominator() == 7L && abs(d$getValue() - 3/7) < 1E-12)
#'  stopifnot(class(d)[1] == "rationalR6" && is(d, "rationalR6") && is(d, "R6"))
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
  } else
  {
   stop(.rationalErrorMessage5)
  }
}

#' @rdname rational-class
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S4")
#'   stopifnot(length(a) == 3)
setMethod("length",
          "rationalS4",
          function(x)
          {
            callNextMethod(x@n)
          })

#' @rdname rational-class
#' @method length rationalS3
#' @export
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S3")
#'   stopifnot(length(a) == 3)
length.rationalS3 <- function(x, ...)
{
  return(length(x$n))
}

#' @rdname rational-class
#' @method length rationalR6
#' @export
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "R6")
#'   stopifnot(length(a) == 3)
length.rationalR6 <- function(x, ...)
{
  return(length(x$getNumerator()))
}

#' @param x the rational number
#' @param i index specifying elements
#' @param j index specifying elements
#' @param ... indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL.
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples). This only works for extracting elements, not for the replacement. See drop for further details.
#' @param value the replacement value
#' @param exact controls partial matching when extracting by character
#' @seealso \code{\link{Extract}} for more full descriptions
#' @export
#' @rdname rational-class
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S4")
#'   stopifnot(a[2]@@n == 5L)
#'   stopifnot(all(a[2:3]@@n == c(5,6)))
setMethod("[",
          "rationalS4",
          function(x, i, j, ..., drop)
          {
            rational(x@n[i], x@d[i], "S4")
          })

#' @rdname rational-class
#' @export
setMethod("[<-",
          "rationalS4",
          function(x, i, j, ..., value)
          {
            x@n[i] <- value@n
            x@d[i] <- value@d
            x@v[i] <- value@v
            return(x)
          })

#' @rdname rational-class
#' @export
setMethod("[[",
          "rationalS4",
          function(x, i, ..., drop)
          {
            rational(x@n[[i]], x@d[[i]], "S4")
          })

#' @rdname rational-class
#' @export
setMethod("[[<-",
          "rationalS4",
          function(x, i, ..., value)
          {
            x@n[[i]] <- value@n
            x@d[[i]] <- value@d
            x@v[[i]] <- value@v
            return(x)
          })

#' @rdname rational-class
#' @export
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S3")
#'   stopifnot(a[2]$n == 5L)
#'   stopifnot(all(a[2:3]$n == c(5,6)))
'[.rationalS3' <- function(x, i, ..., drop = TRUE)
{
  return(rational(x$n[i], x$d[i], "S3"))
}

#' @rdname rational-class
#' @export
'[<-.rationalS3' <- function(x, i, ..., value)
{
  if (!is.rationalS3(value))
  {
    stop(.rationalErrorMessage8)
  }
  x$n[i] <- value$n
  x$d[i] <- value$d
  x$v[i] <- value$v
  return(x)
}

#' @rdname rational-class
#' @export
'[[.rationalS3' <- function(x, i, ..., exact = TRUE)
{
  return(rational(x$n[[i]], x$d[[i]], "S3"))
}

#' @rdname rational-class
#' @export
'[[<-.rationalS3' <- function(x, i, ..., value)
{
  if (!is.rationalS3(value))
  {
    stop(.rationalErrorMessage8)
  }
  x$n[[i]] <- value$n
  x$d[[i]] <- value$d
  x$v[[i]] <- value$v
  return(x)
}

#' @rdname rational-class
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "R6")
#'   stopifnot(a[2]$getNumerator() == 5L)
#'   stopifnot(all(a[2:3]$n == c(5,6)))
#' @export
'[.rationalR6' <- function(x, i, ..., drop = TRUE)
{
  return(rational(x$getNumerator()[i], x$getDenominator()[i], "R6"))
}

'[<-.rationalR6' <- function(x, i, ..., value)
{
  if (!is.rationalR6(value))
  {
    stop(.rationalErrorMessage7)
  }
  x$assign_at(i, value)
  return(x)
}

#' @rdname rational-class
#' @export
'[[.rationalR6' <- function(x, i, ..., exact = TRUE)
{
  return(rational(x$getNumerator()[[i]], x$getDenominator()[[i]], "R6"))
}

'[[<-.rationalR6' <- function(x, i, ..., value)
{
  if (!is.rationalR6(value))
  {
    stop(.rationalErrorMessage7)
  }
  x$assign_at(i, value)
  return(x)
}

################################################################################

.printRational <- function(n, d, v)
{
  print(paste0("(", n, " / ", d, ") = ", v))
}

.catRational <- function(n, d, v)
{
  cat(paste0("(", n, " / ", d, ") = ", v))
}

#' @rdname rational-class
#' @export
setMethod("print", signature = "rationalS4",
          function(x)
          {
            .printRational(x@n, x@d, x@v)
          })

#' @rdname rational-class
#' @param object the object to show
#' @export
setMethod("show", signature = "rationalS4",
          function(object)
          {
            .catRational(object@n, object@d, object@v)
          })

#' @rdname rational-class
#' @method print rationalS3
#' @export
print.rationalS3 <- function(x, ...)
{
  .printRational(x$n, x$d, x$v)
}

#' @rdname rational-class
#' @method print rationalR6
#' @export
print.rationalR6 <- function(x, ...)
{
  .printRational(x$getNumerator(), x$getDenominator(), x$getValue())
}
