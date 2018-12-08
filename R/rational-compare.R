# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include isRational.R

#' @title Rational Number Comparisons
#'
#' @param e1 rational numbers, integers, or numerics
#' @param e2 rational numbers, integers, or numerics
#' @importMethodsFrom methods Compare
#' @name rational-compare
NULL

#' @rdname rational-compare
#' @export
setMethod("Compare", signature = c("rationalS4", "numeric"),
          function(e1, e2)
          {
            if (e2 != as.integer(e2))
            {
              callGeneric(e1@v, e2)
            } else
            {
              callGeneric(e1@n, e2*e1@d)
            }
          }
)

#' @rdname rational-compare
#' @export
setMethod("Compare", signature = c("rationalS4", "integer"),
          function(e1, e2)
          {
            callGeneric(e1@n, e2*e1@d)
          }
)

#' @rdname rational-compare
#' @export
setMethod("Compare", signature = c("numeric", "rationalS4"),
          function(e1, e2)
          {
            if (e1 != as.integer(e1))
            {
              callGeneric(e1, e2@v)
            } else
            {
              callGeneric(e1*e2@d, e2@n)
            }
          }
)

#' @rdname rational-compare
#' @export
setMethod("Compare", signature = c("integer", "rationalS4"),
          function(e1, e2)
          {
            callGeneric(e1*e2@d, e2@n)
          }
)

#' @rdname rational-compare
#' @export
setMethod("Compare", signature = c("rationalS4", "rationalS4"),
          function(e1, e2)
          {
            if (e1@d == e2@d)
            {
              callGeneric(e1@n, e2@n)
            } else
            {
              # give numbers the same denominator to compare
              n1 <- e1@n * e2@d
              n2 <- e2@n * e1@d
              callGeneric(n1, n2)
            }
          }
)

.compare <- function(n1, d1, n2, d2, comp_func)
{
  if (d1 == d2)
  {
    comp_func(n1, n2)
  } else
  {
    comp_func(n1*d2, n2*d1)
  }
}


#' @rdname rational-compare
#' @export
#' @examples
#' a <- rational(1L, 3L, "S3")
#' b <- rational(3L, 4L, "S3")
#' d <- 3L
#' e <-  20.1
#' stopifnot(a != b)
#' stopifnot(!(a == b))
#' stopifnot(a < b)
#' stopifnot(!(a > b))
#' stopifnot(a <= b)
#' stopifnot(!(a >= b))
#' stopifnot(a != d)
#' stopifnot(!(a == d))
#' stopifnot(a < d)
#' stopifnot(!(a > d))
#' stopifnot(a <= d)
#' stopifnot(!(a >= d))
#' stopifnot(a != e)
#' stopifnot(!(a == e))
#' stopifnot(a < e)
#' stopifnot(!(a > e))
#' stopifnot(a <= e)
#' stopifnot(!(a >= e))
Ops.rationalS3 <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    .compare(e1$n, e1$d, e2$n, e2$d, get(.Generic))
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    .compare(e1$n, e1$d, e2, 1L, get(.Generic))
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    .compare(e1, 1L, e2$n, e2$d, get(.Generic))
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    get(.Generic)(e1$v, e2)
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    get(.Generic)(e1, e2$v)
  } else
  {
    stop("Comparisons are only valid between rationalS3, integers, and numerics")
  }
}

#' @rdname rational-compare
#' @export
#' @examples
#' a <- rational(1L, 3L, "R6")
#' b <- rational(3L, 4L, "R6")
#' d <- 3L
#' e <-  20.1
#' stopifnot(a != b)
#' stopifnot(!(a == b))
#' stopifnot(a < b)
#' stopifnot(!(a > b))
#' stopifnot(a <= b)
#' stopifnot(!(a >= b))
#' stopifnot(a != d)
#' stopifnot(!(a == d))
#' stopifnot(a < d)
#' stopifnot(!(a > d))
#' stopifnot(a <= d)
#' stopifnot(!(a >= d))
#' stopifnot(a != e)
#' stopifnot(!(a == e))
#' stopifnot(a < e)
#' stopifnot(!(a > e))
#' stopifnot(a <= e)
#' stopifnot(!(a >= e))
Ops.rationalR6 <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    .compare(e1$getNumerator(), e1$getDenominator(), e2$getNumerator(), e2$getDenominator(), get(.Generic))
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    .compare(e1$getNumerator(), e1$getDenominator(), e2, 1L, get(.Generic))
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    .compare(e1, 1L, e2$getNumerator(), e2$getDenominator(), get(.Generic))
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    get(.Generic)(e1$getValue(), e2)
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    get(.Generic)(e1, e2$getValue())
  } else
  {
    stop("Comparisons are only valid between rationalS3, integers, and numerics")
  }
}