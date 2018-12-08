# include the rational-class.R so that it is loaded first
#' @include rational-class.R

#' @title Rational Number Arithmetic
#'
#' @param e1 rational numbers, integers, or numerics
#' @param e2 rational numbers, integers, or numerics
#' @field add for R6 classes, using the \code{$add(e1)} to do addition is the fastest method
#' @name rational-operators
NULL

# Add two rational numbers
#
# n1 numerator of the first number
# d1 denominator of the first number
# n2 numerator of the second number
# d2 denominator of the second number
# return a list of a resulting numerator and denominator
.rationalAddRational <- function(n1, d1, n2, d2)
{
  if (length(n1) != length(n2))
    stop(.rationalErrorMessage6)
  n <- n1*d2 + n2*d1
  d <- d1*d2
  g <- .gcd(n,d)
  n <- n %/% g
  d <- d %/% g
  return(list(n=n, d=d))
}
.rationalAddInteger <- function(n1, d1, i2) .rationalAddRational(n1, d1, i2, rep(1L, length(i2)))
.rationalAddNumeric <- function(n1, d1, f2) n1 / d1 + f2

# Note that rationalR6 and R6 will not work as signatures for setMethod for
#  "+" because it is a primitive even when they have been declared wtih
#  setOldMethod.  Only formal S4 classes created using setClass will
#  work as primitive method signatures

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- rational(3L,5L,"S4")
#'   d <- a + b
#'   stopifnot(d@@n == 11)
#'   stopifnot(d@@d == 10)
setMethod("+", c("rationalS4", "rationalS4"), function(e1, e2)
{
  res <- .rationalAddRational(e1@n, e1@d, e2@n, e2@d)
  return(new("rationalS4", n=res$n, d=res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- 7L
#'   b <- rational(3L,5L,"S4")
#'   d <- a + b
#'   stopifnot(d@@n == 38)
#'   stopifnot(d@@d == 5)
setMethod("+", c("integer", "rationalS4"), function(e1, e2)
{
  res <- .rationalAddInteger(e2@n, e2@d, e1)
  return(new("rationalS4", n=res$n, d=res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7L
#'   d <- a + b
#'   stopifnot(d@@n == 15)
#'   stopifnot(d@@d == 2)
setMethod("+", c("rationalS4", "integer"), function(e1, e2)
{
  res <- .rationalAddInteger(e1@n, e1@d, e2)
  return(new("rationalS4", n=res$n, d=res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- 7
#'   b <- rational(3L,5L,"S4")
#'   d <- a + b
#'   stopifnot(abs(d - 7.6) < 1E-12)
setMethod("+", c("numeric", "rationalS4"), function(e1, e2)
{
  return(.rationalAddNumeric(e2@n, e2@d, e1))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7
#'   d <- a + b
#'   stopifnot(abs(d - 7.5) < 1E-12)
setMethod("+", c("rationalS4", "numeric"), function(e1, e2)
{
  return(.rationalAddNumeric(e1@n, e1@d, e2))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S3")
#'   b <- rational(3L,5L,"S3")
#'   d <- a + b
#'   stopifnot(d$n == 11)
#'   stopifnot(d$d == 10)
#'   a <- 7L
#'   b <- rational(3L,5L,"S3")
#'   d <- a + b
#'   stopifnot(d$n == 38)
#'   stopifnot(d$d == 5)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7L
#'   d <- a + b
#'   stopifnot(d$n == 15)
#'   stopifnot(d$d == 2)
#'   a <- 7
#'   b <- rational(3L,5L,"S3")
#'   d <- a + b
#'   stopifnot(abs(d - 7.6) < 1E-12)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7
#'   d <- a + b
#'   stopifnot(abs(d - 7.5) < 1E-12)
'+.rationalS3' <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    res <- .rationalAddRational(e1$n, e1$d, e2$n, e2$d)
    return(rationalS3(res$n, res$d))
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    res <- .rationalAddInteger(e2$n, e2$d, e1)
    return(rationalS3(res$n, res$d))
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    res <- .rationalAddInteger(e1$n, e1$d, e2)
    return(rationalS3(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    return(.rationalAddNumeric(e2$n, e2$d, e1))
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    return(.rationalAddNumeric(e1$n, e1$d, e2))
  } else
  {
    return(NA)
  }
}

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   d <- a + b
#'   stopifnot(d$getNumerator() == 11)
#'   stopifnot(d$getDenominator() == 10)
#'   a <- 7L
#'   b <- rational(3L,5L,"R6")
#'   d <- a + b
#'   stopifnot(d$getNumerator() == 38)
#'   stopifnot(d$getDenominator() == 5)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   d <- a + b
#'   stopifnot(d$getNumerator() == 15)
#'   stopifnot(d$getDenominator() == 2)
#'   a <- 7
#'   b <- rational(3L,5L,"R6")
#'   d <- a + b
#'   stopifnot(abs(d - 7.6) < 1E-12)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7
#'   d <- a + b
#'   stopifnot(abs(d - 7.5) < 1E-12)
'+.rationalR6' <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    res <- .rationalAddRational(e1$getNumerator(), e1$getDenominator(), e2$getNumerator(), e2$getDenominator())
    return(rationalR6(res$n, res$d))
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    res <- .rationalAddInteger(e2$getNumerator(), e2$getDenominator(), e1)
    return(rationalR6(res$n, res$d))
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    res <- .rationalAddInteger(e1$getNumerator(), e1$getDenominator(), e2)
    return(rationalR6(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    return(.rationalAddNumeric(e2$getNumerator(), e2$getDenominator(), e1))
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    return(.rationalAddNumeric(e1$getNumerator(), e1$getDenominator(), e2))
  } else
  {
    return(NA)
  }
}

#' @name R6Class$add
#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   a$add(b)
#'   stopifnot(a$getNumerator() == 11)
#'   stopifnot(a$getDenominator() == 10)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   a$add(b)
#'   stopifnot(a$getNumerator() == 15)
#'   stopifnot(a$getDenominator() == 2)
.rationalR6$set("public", "add", function(e1)
{
  if (is.rationalR6(e1))
  {
    res <- .rationalAddRational(private$n, private$d, e1$getNumerator(), e1$getDenominator())
  } else if (is.integer(e1))
  {
    res <- .rationalAddInteger(private$n, private$d, e1)
  } else
  {
    stop("R6$add can only be used with objects of class rationalR6 and integer")
  }
  private$n <- res$n
  private$d <- res$d
  private$v <- private$n / private$d
  return(self)
})
