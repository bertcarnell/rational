# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include isRational.R
#' @include rational-group-generics.R

#' @title Rational Number Comparisons
#'
#' @param e1 rational numbers, integers, or numerics
#' @param e2 rational numbers, integers, or numerics
#' @param ... Additional arguments
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
  if (length(n1) != length(n2))
    stop("Comparison of rational objects of different lengths is not supported")
  comp_func(n1*d2, n2*d1)
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

# methods::getGroupMembers("Compare")
# S7:::base_ops # version 0.1.1 attempt at this

# @rdname rational-compare
# @usage \method{Compare}{rationalS7,rationalS7}(e1, e2)
# @name rationalS7_gt

S7::method(S7_Compare, list(rational:::rationalS7, rational:::rationalS7)) <- function(e1, e2, ..., .Generic) {
 .Generic <- find_base_generic(.Generic)
 .compare(e1@n, e1@d, e2@n, e2@d, .Generic)
}

S7::method(S7_Compare, list(S7::class_integer, rational:::rationalS7)) <- function(e1, e2, ..., .Generic) {
  .Generic <- find_base_generic(.Generic)
  .compare(e1, 1L, e2@n, e2@d, .Generic)
}

S7::method(S7_Compare, list(rational:::rationalS7, S7::class_integer)) <- function(e1, e2, ..., .Generic) {
  .Generic <- find_base_generic(.Generic)
  .compare(e1@n, e1@d, e2, 1L, .Generic)
}

S7::method(S7_Compare, list(S7::class_double, rational:::rationalS7)) <- function(e1, e2, ..., .Generic) {
  .Generic <- find_base_generic(.Generic)
  .Generic(e1, e2@v)
}

S7::method(S7_Compare, list(rational:::rationalS7, S7::class_double)) <- function(e1, e2, ..., .Generic) {
  .Generic <- find_base_generic(.Generic)
  .Generic(e1@v, e2)
}

#' @rdname rational-compare
#' @export
#'
#' @importFrom methods getGroupMembers
#'
#' @note Dispatch happens in 3 steps
#' \itemize{
#' \item 1. Ops.rational::rationalS7
#' \item 2. S7_Ops generic
#' \item 3. rationalS7 method for S7_Ops with the right signature
#' }
#'
#' @examples
#' a <- rational(1L, 3L, "S7")
#' b <- rational(3L, 4L, "S7")
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
'Ops.rational::rationalS7' <- function(e1, e2, ...) {
  if (.Generic %in% methods::getGroupMembers("Compare"))
  {
    #S7_Ops(e1, e2, ..., .Generic = .Generic)
    S7_Compare(e1, e2, ..., .Generic = .Generic)
  } else if (.Generic == "+")
  {
    S7_Add(e1, e2, ..., .Generic = .Generic)
  } else if (.Generic == "-")
  {
    S7_Subtract(e1, e2, ..., .Generic = .Generic)
  } else if (.Generic == "*")
  {
    S7_Multiply(e1, e2, ..., .Generic = .Generic)
  } else if (.Generic == "/")
  {
    S7_Divide(e1, e2, ..., .Generic = .Generic)
  } else if (.Generic == "%/%")
  {
    S7_Integer_Divide(e1, e2, ..., .Generic = .Generic)
  } else if (.Generic == "%%")
  {
    return(e1 - e2 * e1 %/% e2)
  } else if (.Generic %in% methods::getGroupMembers("Arith")) {
    S7_Ops(e1, e2, ..., .Generic = .Generic)
  } else {
    stop(paste0("Function ", .Generic, " not available for class rationalS7"))
  }
}
