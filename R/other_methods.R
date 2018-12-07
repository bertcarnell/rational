# include the rational-class.R so that it is loaded first
#' @include rational-class.R

#' @rdname rational-class
#' @examples
#' floor(rational(5L, 2L, "S4"))
#' sign(rational(-1L, 2L, "S4"))
setMethod("Math", signature = c("rationalS4"),
  function(x)
  {
    callGeneric(x@v)
  }
)

#' @rdname rational-class
#' @examples
#' round(rational(5L, 2L, "S4"), 0)
setMethod("Math2", signature = c("rationalS4"),
  function(x)
  {
    callGeneric(x@v)
  }
)

#' @rdname rational-class
#' @examples
#' x <- abs(rational(-2L, 3L, "S4"))
#' stopifnot("rationalS4" %in% class(x))
setMethod("abs", signature = c("rationalS4"),
  function(x)
  {
    n <- abs(x@n)
    d <- abs(x@d)
    rational(n, d, "S4")
  }
)

#' @rdname rational-class
#' @examples
#' log(rational(2L, 5L, "S4"))
setMethod("log", signature = c("rationalS4"),
  function(x)
  {
    log(x@n) - log(x@d)
  }
)

#' @rdname rational-class
#' @examples
#' log10(rational(2L, 5L, "S4"))
setMethod("log10", signature = c("rationalS4"),
  function(x)
  {
    log10(x@n) - log10(x@d)
  }
)

setGeneric("logb")

#' @rdname rational-class
#' @examples
#' logb(rational(2L, 5L, "S4"), base = 5)
setMethod("logb", signature = c("rationalS4"),
  function(x, base)
  {
    logb(x@n, base = base) - logb(x@d, base = base)
  }
)

#' @rdname rational-class
#' @examples
#' log2(rational(2L, 5L, "S4"))
setMethod("log2", signature = c("rationalS4"),
  function(x)
  {
    log2(x@n) - log2(x@d)
  }
)

#' @rdname rational-class
#' @examples
#' gamma(rational(2L, 5L, "S4"))
setMethod("gamma", signature = c("rationalS4"),
  function(x)
  {
    if (x@d == 2 && x@n %% 2 == 1)
    {
      # gamma(n+1/2) = choose(n-1/2,n)n!sqrt(pi)
      if (sign(x@n) == 1)
      {
        n <- (x@n - 1) %/% 2
        choose(n - 0.5, n) * factorial(n) * sqrt(pi)
      } else
      {
        n <- -(x@n - 1) %/% 2
        sqrt(pi) / choose(-0.5, n) / factorial(n)
      }
    } else
    {
      gamma(x@v)
    }
  }
)

#' @rdname rational-class
#' @examples
#' max(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("max", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    m <- which.max(x@v)
    rational(x@n[m], x@d[m], "S4")
  }
)

#' @rdname rational-class
#' @examples
#' min(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("min", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    m <- which.min(x@v)
    rational(x@n[m], x@d[m], "S4")
  }
)

#' @rdname rational-class
#' @examples
#' range(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("range", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    c(min(x), max(x))
  }
)

#' @rdname rational-class
#' @examples
#' prod(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("prod", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    n <- prod(x@n)
    d <- prod(x@d)
    if (n > .Machine$integer.max || d > .Machine$integer.max)
    {
      warning("Numerator or denominator are too large, converting to numeric")
      return(n / d)
    } else
    {
      return(rational(as.integer(n), as.integer(d), "S4"))
    }
  }
)

#' @rdname rational-class
#' @examples
#' sum(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("sum", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    d <- prod(x@d)
    n <- sum(x@n * d %/% x@d)
    if (n > .Machine$integer.max || d > .Machine$integer.max)
    {
      warning("Numerator or denominator are too large, converting to numeric")
      return(n / d)
    } else
    {
      g <- .gcd(n, d)
      return(rational(as.integer(n %/% g), as.integer(d %/% g), "S4"))
    }
  }
)

