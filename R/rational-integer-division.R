# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include rational-add.R

.rationalIntDivRational <- function(n1, d1, n2, d2) (n1 * d2) %/% (n2 * d1)
.integerIntDivRational <- function(i1, n2, d2) (i1 * d2) %/% n2
.rationalIntDivInteger <- function(n1, d1, i2) n1 %/% (i2 * d1)
.rationalIntDivNumeric <- function(v1, v2) v1 %/% v2
.numericIntDivRational <- function(v1, v2) v1 %/% v2

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(3L,1L,"S4")
#'   b <- rational(2L,1L,"S4")
#'   a %/% b
setMethod("%/%", signature = c("rationalS4", "rationalS4"),
          function(e1,e2)
          {
            .rationalIntDivRational(e1@n, e1@d, e2@n, e2@d)
          }
)

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- 3L
#'   b <- rational(2L,1L,"S4")
#'   a %/% b
setMethod("%/%", signature = c("integer", "rationalS4"),
          function(e1,e2)
          {
            .integerIntDivRational(e1, e2@n, e2@d)
          }
)

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(3L,1L,"S4")
#'   b <- 2L
#'   a %/% b
setMethod("%/%", signature = c("rationalS4", "integer"),
          function(e1,e2)
          {
            .rationalIntDivInteger(e1@n, e1@d, e2)
          }
)

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- 3.5
#'   b <- rational(2L,1L,"S4")
#'   a %/% b
setMethod("%/%", signature = c("numeric", "rationalS4"),
          function(e1,e2)
          {
            .numericIntDivRational(e1, e2@v)
          }
)

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(3L,1L,"S4")
#'   b <- 2.1
#'   a %/% b
setMethod("%/%", signature = c("rationalS4", "numeric"),
          function(e1,e2)
          {
            .rationalIntDivNumeric(e1@v, e2)
          }
)

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S3")
#'   b <- rational(3L,5L,"S3")
#'   d <- 3L
#'   e <- 1.5
#'   a %/% b
#'   d %/% b
#'   a %/% d
#'   e %/% b
#'   d %/% e
'%/%.rationalS3' <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    res <- .rationalIntDivRational(e1$n, e1$d, e2$n, e2$d)
    return(res)
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    res <- .integerIntDivRational(e1, e2$n, e2$d)
    return(res)
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    res <- .rationalIntDivInteger(e1$n, e1$d, e2)
    return(res)
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    return(.numericIntDivRational(e1, e2$v))
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    return(.rationalIntDivNumeric(e1$v, e2))
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
#'   d <- 3L
#'   e <- 1.5
#'   a %/% b
#'   d %/% b
#'   a %/% d
#'   e %/% b
#'   d %/% e
'%/%.rationalR6' <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    res <- .rationalIntDivRational(e1$getNumerator(), e1$getDenominator(),
                                e2$getNumerator(), e2$getDenominator())
    return(res)
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    res <- .integerIntDivRational(e1, e2$getNumerator(), e2$getDenominator())
    return(res)
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    res <- .rationalIntDivInteger(e1$getNumerator(), e1$getDenominator(), e2)
    return(res)
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    return(.numericIntDivRational(e1, e2$getValue()))
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    return(.rationalIntDivNumeric(e1$getValue(), e2))
  } else
  {
    return(NA)
  }
}

