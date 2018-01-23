# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include gcd.R
#' @include rational-add.R
#' @include isRational.R

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- rational(3L,5L,"S4")
#'   d <- a - b
#'   stopifnot(d@@n == -1)
#'   stopifnot(d@@d == 10)
setMethod("-", c("rationalS4", "rationalS4"), function(e1, e2)
{
  res <- .rationalAddRational(e1@n, e1@d, -1L*e2@n, e2@d)
  return(new("rationalS4", n=res$n, d=res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- 7L
#'   b <- rational(3L,5L,"S4")
#'   d <- a - b
#'   stopifnot(d@@n == 32)
#'   stopifnot(d@@d == 5)
setMethod("-", c("integer", "rationalS4"), function(e1, e2)
{
  res <- .rationalAddInteger(-1L*e2@n, e2@d, e1)
  return(new("rationalS4", n=res$n, d=res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7L
#'   d <- a + b
#'   stopifnot(d@@n == -13)
#'   stopifnot(d@@d == 2)
setMethod("-", c("rationalS4", "integer"), function(e1, e2)
{
  res <- .rationalAddInteger(e1@n, e1@d, -1L*e2)
  return(new("rationalS4", n=res$n, d=res$d))
})

#' @rdname rational-operators
#' @examples
#'   a <- 7
#'   b <- rational(3L,5L,"S4")
#'   d <- a + b
#'   stopifnot(abs(d - 6.4) < 1E-12)
setMethod("-", c("numeric", "rationalS4"), function(e1, e2)
{
  return(.rationalAddNumeric(-1L*e2@n, e2@d, e1))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 7
#'   d <- a + b
#'   stopifnot(abs(d - -6.5) < 1E-12)
setMethod("+", c("rationalS4", "numeric"), function(e1, e2)
{
  return(.rationalAddNumeric(e1@n, e1@d, -1L*e2))
})

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"S3")
#'   b <- rational(3L,5L,"S3")
#'   d <- a - b
#'   stopifnot(d$n == -1)
#'   stopifnot(d$d == 10)
#'   a <- 7L
#'   b <- rational(3L,5L,"S3")
#'   d <- a - b
#'   stopifnot(d$n == 32)
#'   stopifnot(d$d == 5)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7L
#'   d <- a - b
#'   stopifnot(d$n == -13)
#'   stopifnot(d$d == 2)
#'   a <- 7
#'   b <- rational(3L,5L,"S3")
#'   d <- a - b
#'   stopifnot(abs(d - 6.4) < 1E-12)
#'   a <- rational(1L,2L,"S3")
#'   b <- 7
#'   d <- a + b
#'   stopifnot(abs(d - -6.5) < 1E-12)
'-.rationalS3' <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    res <- .rationalAddRational(e1$n, e1$d, -1L*e2$n, e2$d)
    return(rationalS3(res$n, res$d))
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    res <- .rationalAddInteger(-1L*e2$n, e2$d, e1)
    return(rationalS3(res$n, res$d))
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    res <- .rationalAddInteger(e1$n, e1$d, -1L*e2)
    return(rationalS3(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    return(.rationalAddNumeric(-1L*e2$n, e2$d, e1))
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    return(.rationalAddNumeric(e1$n, e1$d, -1L*e2))
  } else 
  {
    return(NA)
  }
}

#' @rdname rational-operators
#' @examples
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   d <- a - b
#'   stopifnot(d$getNumerator() == -1)
#'   stopifnot(d$getDenominator() == 10)
#'   a <- 7L
#'   b <- rational(3L,5L,"R6")
#'   d <- a - b
#'   stopifnot(d$getNumerator() == 32)
#'   stopifnot(d$getDenominator() == 5)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   d <- a - b
#'   stopifnot(d$getNumerator() == -13)
#'   stopifnot(d$getDenominator() == 2)
#'   a <- 7
#'   b <- rational(3L,5L,"R6")
#'   d <- a - b
#'   stopifnot(abs(d - 6.4) < 1E-12)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7
#'   d <- a - b
#'   stopifnot(abs(d - -6.5) < 1E-12)
'-.rationalR6' <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    res <- .rationalAddRational(e1$getNumerator(), e1$getDenominator(), -1L*e2$getNumerator(), e2$getDenominator())
    return(rationalR6(res$n, res$d))
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    res <- .rationalAddInteger(-1L*e2$getNumerator(), e2$getDenominator(), e1)
    return(rationalR6(res$n, res$d))
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    res <- .rationalAddInteger(e1$getNumerator(), e1$getDenominator(), -1L*e2)
    return(rationalR6(res$n, res$d))
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    return(.rationalAddNumeric(-1L*e2$getNumerator(), e2$getDenominator(), e1))
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    return(.rationalAddNumeric(e1$getNumerator(), e1$getDenominator(), -1*e2))
  } else 
  {
    return(NA)
  }
}

#' @name R6Class$subtract
#' @rdname rational-operators
#' @examples 
#'   a <- rational(1L,2L,"R6")
#'   b <- rational(3L,5L,"R6")
#'   a$subtract(b)
#'   stopifnot(a$getNumerator() == -1)
#'   stopifnot(a$getDenominator() == 10)
#'   a <- rational(1L,2L,"R6")
#'   b <- 7L
#'   a$subtract(b)
#'   stopifnot(a$getNumerator() == -13)
#'   stopifnot(a$getDenominator() == 2)
.rationalR6$set("public", "subtract", function(e1)
{
  if (is.rationalR6(e1))
  {
    res <- .rationalAddRational(-1L*private$n, private$d, e1$getNumerator(), e1$getDenominator())  
  } else if (is.integer(e1))
  {
    res <- .rationalAddInteger(private$n, private$d, -1L*e1)
  } else
  {
    stop("R6$subtract can only be used with objects of class rationalR6 and integer")
  }
  private$n <- res$n
  private$d <- res$d
  private$v <- private$n / private$d
  return(self)
})
