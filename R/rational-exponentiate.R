# include the rational-class.R so that it is loaded first
#' @include rational-class.R

# Note is.integer(3L^5L) == FALSE
# and is.integer(prod(c(2L, 2L, 2L))) == FALSE
# however, is.integer(sum(c(3L, 4L, 5L))) == TRUE
# ?integer does not include infromation about L notation, e.g. is.integer(1L) # is TRUE
# interestingly.. is.integer(sum(c(3L, 4L, 5L))) == TRUE

.integerExponentiate <- function(b, e, checkOverflow=FALSE, USE.NAMES=FALSE)
{
  if (!all(is.integer(b)) || !all(is.integer(e)))
    stop("Function must take integer arguments")
  if (checkOverflow)
  {
    # check to see if we will have integer overflow
    res <- b^e
    if (any(res > .Machine$integer.max))
    {
      return(res)
    }
  }
  tempf <- function(b_, e_)
  {
    if (e_ == 1L)
      return(b_)
    res <- b_
    for (i in 2:e_)
    {
      res <- res * b_
    }
    return(res)
  }
  mapply(tempf, b, e, USE.NAMES = USE.NAMES)
}

.rationalExpRational <- function(n1, d1, v1, n2, d2, v2)
{
  # check overflow
  res_n <- n1^v2
  res_d <- d1^v2
  if (res_n > .Machine$integer.max ||
        res_d > .Machine$integer.max)
  {
    return(v1^v2)
  }
  if (all(d2 == 1L))
  {
    n <- .integerExponentiate(n1, n2, FALSE)
    d <- .integerExponentiate(d1, n2, FALSE)
    return(list(n = n, d = d))
  }
  return(v1^v2)
}

.rationalExpInteger <- function(n1, d1, v1, i2)
{
  # check overflow
  res_n <- n1^i2
  res_d <- d1^i2
  if (res_n > .Machine$integer.max ||
        res_d > .Machine$integer.max)
  {
    return(v1^i2)
  }
  n <- .integerExponentiate(n1, i2, FALSE)
  d <- .integerExponentiate(d1, i2, FALSE)
  return(list(n = n, d = d))
}

.integerExpRational <- function(i1, n2, d2, v2)
{
  # check overflow
  res_n <- i1^v2
  if (res_n > .Machine$integer.max)
  {
    return(res_n)
  }
  if (d2 == 1L)
  {
    n <- .integerExponentiate(i1, n2, FALSE)
    return(list(n = n, d = rep(1L, length(n))))
  }
  return(res_n)
}

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- rational(3L,5L,"S4")
#'   a ^ b
setMethod("^", signature = c("rationalS4", "rationalS4"),
          function(e1,e2)
          {
            res <- .rationalExpRational(e1@n, e1@d, e1@v, e2@n, e2@d, e2@v)
            if (is.list(res))
            {
              return(rational(res$n, res$d, "S4"))
            }
            return(res)
          })

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 3L
#'   a ^ b
setMethod("^", signature = c("rationalS4", "integer"),
          function(e1,e2)
          {
            res <- .rationalExpInteger(e1@n, e1@d, e1@v, e2)
            if (is.list(res))
            {
              return(rational(res$n, res$d, "S4"))
            }
            return(res)
          })

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- 2L
#'   b <- rational(3L,5L,"S4")
#'   a ^ b
setMethod("^", signature = c("integer", "rationalS4"),
          function(e1,e2)
          {
            res <- .integerExpRational(e1, e2@n, e2@d, e2@v)
            if (is.list(res))
            {
              return(rational(res$n, res$d, "S4"))
            }
            return(res)
          })

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S4")
#'   b <- 1.5
#'   a ^ b
setMethod("^", signature = c("rationalS4", "numeric"),
          function(e1,e2)
          {
            return(e1@v^e2)
          })

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- 3.1
#'   b <- rational(3L,5L,"S4")
#'   a ^ b
setMethod("^", signature = c("numeric", "rationalS4"),
          function(e1,e2)
          {
            return(e1^e2@v)
          })

#' @rdname rational-operators
#' @export
#' @examples
#'   a <- rational(1L,2L,"S3")
#'   b <- rational(3L,5L,"S3")
#'   d <- 3L
#'   e <- 1.5
#'   a ^ b
#'   d ^ b
#'   a ^ d
#'   e ^ b
#'   d ^ e
'^.rationalS3' <- function(e1, e2)
{
  if (is.rationalS3(e1) && is.rationalS3(e2))
  {
    res <- .rationalExpRational(e1$n, e1$d, e1$v, e2$n, e2$d, e2$v)
    if (is.list(res))
    {
      return(rational(res$n, res$d, "S3"))
    }
    return(res)
  } else if (is.integer(e1) && is.rationalS3(e2))
  {
    res <- .integerExpRational(e1, e2$n, e2$d, e2$v)
    if (is.list(res))
    {
      return(rational(res$n, res$d, "S3"))
    }
    return(res)
  } else if (is.rationalS3(e1) && is.integer(e2))
  {
    res <- .rationalExpInteger(e1$n, e1$d, e1$v, e2)
    if (is.list(res))
    {
      return(rational(res$n, res$d, "S3"))
    }
    return(res)
  } else if (is.numeric(e1) && is.rationalS3(e2))
  {
    return(e1^e2$v)
  } else if (is.rationalS3(e1) && is.numeric(e2))
  {
    return(e1$v^e2)
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
#'   a ^ b
#'   d ^ b
#'   a ^ d
#'   e ^ b
#'   d ^ e
'^.rationalR6' <- function(e1, e2)
{
  if (is.rationalR6(e1) && is.rationalR6(e2))
  {
    res <- .rationalExpRational(e1$getNumerator(), e1$getDenominator(), e1$getValue(),
                                e2$getNumerator(), e2$getDenominator(), e2$getValue())
    if (is.list(res))
    {
      return(rational(res$n, res$d, "R6"))
    }
    return(res)
  } else if (is.integer(e1) && is.rationalR6(e2))
  {
    res <- .integerExpRational(e1, e2$getNumerator(), e2$getDenominator(), e2$getValue())
    if (is.list(res))
    {
      return(rational(res$n, res$d, "R6"))
    }
    return(res)
  } else if (is.rationalR6(e1) && is.integer(e2))
  {
    res <- .rationalExpInteger(e1$getNumerator(), e1$getDenominator(), e1$getValue(), e2)
    if (is.list(res))
    {
      return(rational(res$n, res$d, "R6"))
    }
    return(res)
  } else if (is.numeric(e1) && is.rationalR6(e2))
  {
    return(e1^e2$getValue())
  } else if (is.rationalR6(e1) && is.numeric(e2))
  {
    return(e1$getValue()^e2)
  } else
  {
    return(NA)
  }
}
