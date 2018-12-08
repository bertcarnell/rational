# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include gcd.R
#' @include rational-multiply.R
#' @include isRational.R

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- rational(3L,5L,"S4")
#'   d <- a / b
#'   stopifnot(d@@n == 5)
#'   stopifnot(d@@d == 6)
setMethod("/", c("rationalS4", "rationalS4"), function(e1, e2)
{
  res <- .rationalMultiplyRational(e1@n, e1@d, e2@d, e2@n)
  return(new("rationalS4", n = res$n, d = res$d))
})

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- 7L
#'   b <- rational(3L,5L,"S4")
#'   d <- a / b
#'   stopifnot(d@@n == 35)
#'   stopifnot(d@@d == 3)
setMethod("/", c("integer", "rationalS4"), function(e1, e2)
{
  res <- .rationalMultiplyInteger(e2@d, e2@n, e1)
  return(new("rationalS4", n = res$n, d = res$d))
})

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7L
#'   d <- a / b
#'   stopifnot(d@@n == 1)
#'   stopifnot(d@@d == 14)
setMethod("/", c("rationalS4", "integer"), function(e1, e2)
{
  res <- .rationalMultiplyInteger(e1@n, e1@d*e2, 1L)
  return(new("rationalS4", n = res$n, d = res$d))
})

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- 7
#'   b <- rational(3L,5L,"S4")
#'   d <- a / b
#'   stopifnot(abs(d - 7*5/3) < 1E-12)
setMethod("/", c("numeric", "rationalS4"), function(e1, e2)
{
  return(.rationalMultiplyNumeric(e2@d, e2@n, e1))
})

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7
#'   d <- a / b
#'   stopifnot(abs(d - 1/14) < 1E-12)
setMethod("/", c("rationalS4", "numeric"), function(e1, e2)
{
  return(.rationalMultiplyNumeric(e1@n, e1@d, 1/e2))
})

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S3")
#'   b <- rational(3L,5L,"S3")
#'   d <- a / b
#'   stopifnot(d$n == 5)
#'   stopifnot(d$d == 6)
#'   a <- 7L
#'   b <- rational(3L,5L,"S3")
#'   d <- a / b
#'   stopifnot(d$n == 35)
#'   stopifnot(d$d == 3)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7L
#'   d <- a / b
#'   stopifnot(d$n == 1)
#'   stopifnot(d$d == 14)
#'   a <- 7
#'   b <- rational(3L,5L,"S3")
#'   d <- a / b
#'   stopifnot(abs(d - 7*5/3) < 1E-12)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7
#'   d <- a / b
#'   stopifnot(abs(d - 1/14) < 1E-12)
'/.rationalS3' <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    res <- .rationalMultiplyRational(e1$n, e1$d, e2$d, e2$n)
    return(.rationalS3(res$n, res$d))
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    res <- .rationalMultiplyInteger(e2$d, e2$n, e1)
    return(.rationalS3(res$n, res$d))
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    res <- .rationalMultiplyInteger(e1$n, e1$d*e2, 1L)
    return(.rationalS3(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    return(.rationalMultiplyNumeric(e2$d, e2$n, e1))
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    return(.rationalMultiplyNumeric(e1$n, e1$d, 1/e2))
  } else
  {
    return(NA)
  }
}

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   d <- a / b
#'   stopifnot(d$getNumerator() == 5)
#'   stopifnot(d$getDenominator() == 6)
#'   a <- 7L
#'   b <- rational(3L,5L,"R6")
#'   d <- a / b
#'   stopifnot(d$getNumerator() == 35)
#'   stopifnot(d$getDenominator() == 3)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   d <- a / b
#'   stopifnot(d$getNumerator() == 1)
#'   stopifnot(d$getDenominator() == 14)
#'   a <- 7
#'   b <- rational(3L,5L,"R6")
#'   d <- a / b
#'   stopifnot(abs(d - 7*5/3) < 1E-12)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7
#'   d <- a / b
#'   stopifnot(abs(d - 1/14) < 1E-12)
'/.rationalR6' <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    res <- .rationalMultiplyRational(e1$getNumerator(), e1$getDenominator(), e2$getDenominator(), e2$getNumerator())
    return(.rationalR6$new(res$n, res$d))
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    res <- .rationalMultiplyInteger(e2$getDenominator(), e2$getNumerator(), e1)
    return(.rationalR6$new(res$n, res$d))
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    res <- .rationalMultiplyInteger(e1$getNumerator(), e1$getDenominator()*e2, 1L)
    return(.rationalR6$new(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    return(.rationalMultiplyNumeric(e2$getDenominator(), e2$getNumerator(), e1))
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    return(.rationalMultiplyNumeric(e1$getNumerator(), e1$getDenominator(), 1/e2))
  } else
  {
    return(NA)
  }
}

#' @name R6Class$divide
#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   a$divide(b)
#'   stopifnot(a$getNumerator() == 5)
#'   stopifnot(a$getDenominator() == 6)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   a$divide(b)
#'   stopifnot(a$getNumerator() == 1)
#'   stopifnot(a$getDenominator() == 14)
.rationalR6$set("public", "divide", function(e1)
{
  if (is.rationalR6(e1))
  {
    res <- .rationalMultiplyRational(private$n, private$d, e1$getDenominator(), e1$getNumerator())
  } else if (is.integer(e1))
  {
    res <- .rationalMultiplyInteger(private$n, private$d*e1, 1L)
  } else
  {
    stop("R6$divide can only be used with objects of class rationalR6 and integer")
  }
  private$n <- res$n
  private$d <- res$d
  private$v <- private$n / private$d
  return(self)
})
