# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include gcd.R
#' @include isRational.R

# multiply two rational numbers
#
# n1 numerator of the first number
# d1 denominator of the first number
# n2 numerator of the second number
# d2 denominator of the second number
# return a list of a resulting numerator and denominator
.rationalMultiplyRational <- function(n1, d1, n2, d2)
{
  if (length(n1) != length(n2))
    stop(.rationalErrorMessage6)
  n <- n1*n2
  d <- d1*d2
  g <- .gcd(n,d)
  n <- n %/% g
  d <- d %/% g
  return(list(n = n, d = d))
}
.rationalMultiplyInteger <- function(n1, d1, i2) .rationalMultiplyRational(n1, d1, i2, rep(1L, length(i2)))
.rationalMultiplyNumeric <- function(n1, d1, f2) n1 / d1 * f2

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- rational(3L,5L,"S4")
#'   d <- a * b
#'   stopifnot(d@@n == 3)
#'   stopifnot(d@@d == 10)
setMethod("*", c("rationalS4", "rationalS4"), function(e1, e2)
{
  res <- .rationalMultiplyRational(e1@n, e1@d, e2@n, e2@d)
  return(new("rationalS4", n = res$n, d = res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- 7L
#'   b <- rational(3L,5L,"S4")
#'   d <- a * b
#'   stopifnot(d@@n == 21)
#'   stopifnot(d@@d == 5)
setMethod("*", c("integer", "rationalS4"), function(e1, e2)
{
  res <- .rationalMultiplyInteger(e2@n, e2@d, e1)
  return(new("rationalS4", n = res$n, d = res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7L
#'   d <- a * b
#'   stopifnot(d@@n == 7)
#'   stopifnot(d@@d == 2)
setMethod("*", c("rationalS4", "integer"), function(e1, e2)
{
  res <- .rationalMultiplyInteger(e1@n, e1@d, e2)
  return(new("rationalS4", n = res$n, d = res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- 7
#'   b <- rational(3L,5L,"S4")
#'   d <- a * b
#'   stopifnot(abs(d - 4.2) < 1E-12)
setMethod("*", c("numeric", "rationalS4"), function(e1, e2)
{
  return(.rationalMultiplyNumeric(e2@n, e2@d, e1))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7
#'   d <- a * b
#'   stopifnot(abs(d - 3.5) < 1E-12)
setMethod("*", c("rationalS4", "numeric"), function(e1, e2)
{
  return(.rationalMultiplyNumeric(e1@n, e1@d, e2))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S3")
#'   b <- rational(3L,5L,"S3")
#'   d <- a * b
#'   stopifnot(d$n == 3)
#'   stopifnot(d$d == 10)
#'   a <- 7L
#'   b <- rational(3L,5L,"S3")
#'   d <- a * b
#'   stopifnot(d$n == 21)
#'   stopifnot(d$d == 5)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7L
#'   d <- a * b
#'   stopifnot(d$n == 7)
#'   stopifnot(d$d == 2)
#'   a <- 7
#'   b <- rational(3L,5L,"S3")
#'   d <- a * b
#'   stopifnot(abs(d - 4.2) < 1E-12)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7
#'   d <- a * b
#'   stopifnot(abs(d - 3.5) < 1E-12)
'*.rationalS3' <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    res <- .rationalMultiplyRational(e1$n, e1$d, e2$n, e2$d)
    return(rationalS3(res$n, res$d))
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    res <- .rationalMultiplyInteger(e2$n, e2$d, e1)
    return(rationalS3(res$n, res$d))
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    res <- .rationalMultiplyInteger(e1$n, e1$d, e2)
    return(rationalS3(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    return(.rationalMultiplyNumeric(e2$n, e2$d, e1))
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    return(.rationalMultiplyNumeric(e1$n, e1$d, e2))
  } else
  {
    return(NA)
  }
}

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   d <- a * b
#'   stopifnot(d$getNumerator() == 3)
#'   stopifnot(d$getDenominator() == 10)
#'   a <- 7L
#'   b <- rational(3L,5L,"R6")
#'   d <- a * b
#'   stopifnot(d$getNumerator() == 21)
#'   stopifnot(d$getDenominator() == 5)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   d <- a * b
#'   stopifnot(d$getNumerator() == 7)
#'   stopifnot(d$getDenominator() == 2)
#'   a <- 7
#'   b <- rational(3L,5L,"R6")
#'   d <- a * b
#'   stopifnot(abs(d - 4.2) < 1E-12)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7
#'   d <- a * b
#'   stopifnot(abs(d - 3.5) < 1E-12)
'*.rationalR6' <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    res <- .rationalMultiplyRational(e1$getNumerator(), e1$getDenominator(), e2$getNumerator(), e2$getDenominator())
    return(rationalR6(res$n, res$d))
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    res <- .rationalMultiplyInteger(e2$getNumerator(), e2$getDenominator(), e1)
    return(rationalR6(res$n, res$d))
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    res <- .rationalMultiplyInteger(e1$getNumerator(), e1$getDenominator(), e2)
    return(rationalR6(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    return(.rationalMultiplyNumeric(e2$getNumerator(), e2$getDenominator(), e1))
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    return(.rationalMultiplyNumeric(e1$getNumerator(), e1$getDenominator(), e2))
  } else
  {
    return(NA)
  }
}

#' @name R6Class$multiply
#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   a$multiply(b)
#'   stopifnot(a$getNumerator() == 3)
#'   stopifnot(a$getDenominator() == 10)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   a$multiply(b)
#'   stopifnot(a$getNumerator() == 7)
#'   stopifnot(a$getDenominator() == 2)
.rationalR6$set("public", "multiply", function(e1)
{
  if (is.rationalR6(e1))
  {
    res <- .rationalMultiplyRational(private$n, private$d, e1$getNumerator(), e1$getDenominator())
  } else if (is.integer(e1))
  {
    res <- .rationalMultiplyInteger(private$n, private$d, e1)
  } else
  {
    stop("R6$multiply can only be used with objects of class rationalR6 and integer")
  }
  private$n <- res$n
  private$d <- res$d
  private$v <- private$n / private$d
  return(self)
})
