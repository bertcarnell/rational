# include the rational-class.R so that it is loaded first
#' @include rational-class.R

#' @title Functions of Rational Numbers
#'
#' @param x rational numbers
#' @param ... rational numbers
#' @param na.rm will NAs be removed from a collection before the computation is performed
#' @importMethodsFrom methods Math Math2
#' @name rational-functions
NULL

#' @rdname rational-functions
#' @export
#' @examples
#' floor(rational(5L, 2L, "S4"))
#' sign(rational(-1L, 2L, "S4"))
setMethod("Math", signature = c("rationalS4"),
  function(x)
  {
    callGeneric(x@v)
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' exp(rational(1L, 3L, "S3"))
Math.rationalS3 <- function(x, ...)
{
  get(.Generic)(x$v, ...)
}

#' @rdname rational-functions
#' @export
#' @examples
#' exp(rational(1L, 3L, "R6"))
Math.rationalR6 <- function(x, ...)
{
  get(.Generic)(x$getValue(), ...)
}

#' @rdname rational-functions
#' @export
#' @examples
#' exp(rational(1L, 3L, "S7"))
'Math.rational::rationalS7' <- function(x, ...)
{
  get(.Generic)(x@v, ...)
}

#' @rdname rational-functions
#' @export
#' @examples
#' round(rational(5L, 2L, "S4"), 0)
setMethod("Math2", signature = c("rationalS4"),
  function(x)
  {
    callGeneric(x@v)
  }
)

.rational_abs <- function(n, d, rational_type)
{
  rational(abs(n), abs(d), rational_type)
}

#' @rdname rational-functions
#' @export
#' @examples
#' x <- abs(rational(-2L, 3L, "S4"))
#' stopifnot("rationalS4" %in% class(x))
setMethod("abs", signature = c("rationalS4"),
  function(x)
  {
    .rational_abs(x@n, x@d, "S4")
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' x <- abs(rational(-2L, 3L, "S3"))
#' stopifnot("rationalS3" %in% class(x))
abs.rationalS3 <- function(x)
{
  .rational_abs(x$n, x$d, "S3")
}

#' @rdname rational-functions
#' @export
#' @examples
#' x <- abs(rational(-2L, 3L, "R6"))
#' stopifnot("rationalR6" %in% class(x))
abs.rationalR6 <- function(x)
{
  .rational_abs(x$getNumerator(), x$getDenominator(), "R6")
}

#' @rdname rational-functions
#' @export
#' @examples
#' x <- abs(rational(-2L, 3L, "S7"))
#' stopifnot("rational::rationalS7" %in% class(x))
'abs.rational::rationalS7' <- function(x)
{
  .rational_abs(x@n, x@d, "S7")
}

.rational_log <- function(n, d, base)
{
  if (base == exp(1))
    log(n) - log(d)
  else if (base == 10)
    log10(n) - log10(d)
  else if (base == 2)
    log2(n) - log2(d)
  else
    logb(n, base = base) - logb(d, base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log(rational(2L, 5L, "S4"))
setMethod("log", signature = c("rationalS4"),
  function(x, base = exp(1))
  {
    .rational_log(x@n, x@d, base = base)
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' log(rational(2L, 5L, "S3"))
log.rationalS3 <- function(x, base = exp(1))
{
  .rational_log(x$n, x$d, base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log(rational(2L, 5L, "R6"))
log.rationalR6 <- function(x, base = exp(1))
{
  .rational_log(x$getNumerator(), x$getDenominator(), base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log(rational(2L, 5L, "S7"))
'log.rational::rationalS7' <- function(x, base = exp(1))
{
  .rational_log(x@n, x@d, base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log10(rational(2L, 5L, "S4"))
setMethod("log10", signature = c("rationalS4"),
  function(x)
  {
    .rational_log(x@n, x@d, 10)
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' log10(rational(2L, 5L, "S3"))
log10.rationalS3 <- function(x)
{
  .rational_log(x$n, x$d, 10)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log10(rational(2L, 5L, "R6"))
log10.rationalR6 <- function(x)
{
  .rational_log(x$getNumerator(), x$getDenominator(), 10)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log10(rational(2L, 5L, "S7"))
'log10.rational::rationalS7' <- function(x)
{
  .rational_log(x@n, x@d, 10)
}

setGeneric("logb")

#' @rdname rational-functions
#' @param base the logarithm base in the \code{logb} function
#' @export
#' @examples
#' logb(rational(2L, 5L, "S4"), base = 5)
setMethod("logb", signature = c("rationalS4"),
  function(x, base = exp(1))
  {
    .rational_log(x@n, x@d, base = base)
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' logb(rational(2L, 5L, "S3"), base = 5)
logb.rationalS3 <- function(x, base = exp(1))
{
  .rational_log(x$n, x$d, base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' logb(rational(2L, 5L, "R6"), base = 5)
logb.rationalR6 <- function(x, base = exp(1))
{
  .rational_log(x$getNumerator(), x$getDenominator(), base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' logb(rational(2L, 5L, "S7"), base = 5)
'logb.rational::rationalS7' <- function(x, base = exp(1))
{
  .rational_log(x@n, x@d, base = base)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log2(rational(2L, 5L, "S4"))
setMethod("log2", signature = c("rationalS4"),
  function(x)
  {
    .rational_log(x@n, x@d, base = 2)
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' log2(rational(2L, 5L, "S3"))
log2.rationalS3 <- function(x)
{
  .rational_log(x$n, x$d, 2)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log2(rational(2L, 5L, "R6"))
log2.rationalR6 <- function(x)
{
  .rational_log(x$getNumerator(), x$getDenominator(), 2)
}

#' @rdname rational-functions
#' @export
#' @examples
#' log2(rational(2L, 5L, "S7"))
'log2.rational::rationalS7' <- function(x)
{
  .rational_log(x@n, x@d, 2)
}

#' @rdname rational-functions
#' @export
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

#' @rdname rational-functions
#' @export
#' @examples
#' gamma(rational(2L, 5L, "S3"))
gamma.rationalS3 <- function(x)
{
  gamma(x$v)
}

#' @rdname rational-functions
#' @export
#' @examples
#' gamma(rational(2L, 5L, "R6"))
gamma.rationalR6 <- function(x)
{
  gamma(x$getValue())
}

#' @rdname rational-functions
#' @export
#' @examples
#' gamma(rational(2L, 5L, "S7"))
'gamma.rational::rationalS7' <- function(x)
{
  gamma(x@v)
}

.rational_max <- function(n, d, v, na.rm = FALSE, rational_type = "S4")
{
  if (!na.rm && any(is.na(v)))
    return(rational(as.integer(NA), 1L, rational_type))
  m <- which.max(v)
  rational(as.integer(n[m]), as.integer(d[m]), rational_type)
}

.rational_min <- function(n, d, v, na.rm = FALSE, rational_type = "S4")
{
  if (!na.rm && any(is.na(v)))
    return(rational(as.integer(NA), 1L, rational_type))
  m <- which.min(v)
  rational(as.integer(n[m]), as.integer(d[m]), rational_type)
}

#' @rdname rational-functions
#' @export
#' @examples
#' max(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("max", signature = c(x = "rationalS4"),
  function(x, ..., na.rm=FALSE)
  {
    args_ <- list(...)
    if (length(args_) > 0)
    {
      n <- c(x@n, sapply(args_, function(z) z@n, USE.NAMES = FALSE))
      d <- c(x@d, sapply(args_, function(z) z@d, USE.NAMES = FALSE))
      v <- c(x@v, sapply(args_, function(z) z@v, USE.NAMES = FALSE))
    } else
    {
      n <- x@n
      d <- x@d
      v <- x@v
    }
    .rational_max(n, d, v, na.rm, "S4")
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' max(rational(c(2L, 3L), c(5L, 1L), "S3"))
max.rationalS3 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$d, USE.NAMES = FALSE)
  v <- sapply(args_, function(z) z$v, USE.NAMES = FALSE)
  .rational_max(n, d, v, na.rm = na.rm, "S3")
}

#' @rdname rational-functions
#' @export
#' @examples
#' max(rational(c(2L, 3L), c(5L, 1L), "R6"))
max.rationalR6 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$getNumerator(), USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$getDenominator(), USE.NAMES = FALSE)
  v <- sapply(args_, function(z) z$getValue(), USE.NAMES = FALSE)
  .rational_max(n, d, v, na.rm = na.rm, "R6")
}

#' @rdname rational-functions
#' @export
#' @examples
#' max(rational(c(2L, 3L), c(5L, 1L), "S7"))
'max.rational::rationalS7' <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z@n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z@d, USE.NAMES = FALSE)
  v <- sapply(args_, function(z) z@v, USE.NAMES = FALSE)
  .rational_max(n, d, v, na.rm = na.rm, "S7")
}

#' @rdname rational-functions
#' @export
#' @examples
#' min(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("min", signature = c(x = "rationalS4"),
  function(x, ..., na.rm=FALSE)
  {
    args_ <- list(...)
    if (length(args_) > 0)
    {
      n <- c(x@n, sapply(args_, function(z) z@n, USE.NAMES = FALSE))
      d <- c(x@d, sapply(args_, function(z) z@d, USE.NAMES = FALSE))
      v <- c(x@v, sapply(args_, function(z) z@v, USE.NAMES = FALSE))
    } else
    {
      n <- x@n
      d <- x@d
      v <- x@v
    }
    .rational_min(n, d, v, na.rm, "S4")
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' min(rational(c(2L, 3L), c(5L, 1L), "S3"))
min.rationalS3 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$d, USE.NAMES = FALSE)
  v <- sapply(args_, function(z) z$v, USE.NAMES = FALSE)
  .rational_min(n, d, v, na.rm = na.rm, "S3")
}

#' @rdname rational-functions
#' @export
#' @examples
#' min(rational(c(2L, 3L), c(5L, 1L), "R6"))
min.rationalR6 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$getNumerator(), USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$getDenominator(), USE.NAMES = FALSE)
  v <- sapply(args_, function(z) z$getValue(), USE.NAMES = FALSE)
  .rational_min(n, d, v, na.rm = na.rm, "R6")
}

#' @rdname rational-functions
#' @export
#' @examples
#' min(rational(c(2L, 3L), c(5L, 1L), "S7"))
'min.rational::rationalS7' <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z@n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z@d, USE.NAMES = FALSE)
  v <- sapply(args_, function(z) z@v, USE.NAMES = FALSE)
  .rational_min(n, d, v, na.rm = na.rm, "S7")
}

#' @rdname rational-functions
#' @export
#' @examples
#' range(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("range", signature = c("rationalS4"),
  function(x, ..., na.rm=FALSE)
  {
    list(min(x, ..., na.rm = na.rm), max(x, ..., na.rm = na.rm))
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' range(rational(c(2L, 3L), c(5L, 1L), "S3"))
range.rationalS3 <- function(..., na.rm = FALSE)
{
  list(min(..., na.rm = na.rm), max(..., na.rm = na.rm))
}

#' @rdname rational-functions
#' @export
#' @examples
#' range(rational(c(2L, 3L), c(5L, 1L), "R6"))
range.rationalR6 <- function(..., na.rm = FALSE)
{
  list(min(..., na.rm = na.rm), max(..., na.rm = na.rm))
}

#' @rdname rational-functions
#' @export
#' @examples
#' range(rational(c(2L, 3L), c(5L, 1L), "S7"))
'range.rational::rationalS7' <- function(..., na.rm = FALSE)
{
  list(min(..., na.rm = na.rm), max(..., na.rm = na.rm))
}

.rational_prod <- function(n, d, na.rm = FALSE, rational_type = "S4")
{
  n_ <- prod(n, na.rm = na.rm)
  d_ <- prod(d, na.rm = na.rm)
  if (n_ > .Machine$integer.max || d_ > .Machine$integer.max)
  {
    warning("Numerator or denominator are too large, converting to numeric")
    return(n_ / d_)
  } else
  {
    return(rational(as.integer(n_), as.integer(d_), rational_type))
  }
}

#' @rdname rational-functions
#' @export
#' @examples
#' prod(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("prod", signature = c(x = "rationalS4"),
  function(x, ..., na.rm=FALSE)
  {
    args_ <- list(...)
    if (length(args_) > 0)
    {
      n <- c(x@n, sapply(args_, function(z) z@n, USE.NAMES = FALSE))
      d <- c(x@d, sapply(args_, function(z) z@d, USE.NAMES = FALSE))
    } else
    {
      n <- x@n
      d <- x@d
    }
    .rational_prod(n, d, na.rm, "S4")
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' prod(rational(c(2L, 3L), c(5L, 1L), "S3"))
prod.rationalS3 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$d, USE.NAMES = FALSE)
  .rational_prod(n, d, na.rm, "S3")
}

#' @rdname rational-functions
#' @export
#' @examples
#' prod(rational(c(2L, 3L), c(5L, 1L), "R6"))
prod.rationalR6 <- function(..., na.rm  = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$getNumerator(), USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$getDenominator(), USE.NAMES = FALSE)
  .rational_prod(n, d, na.rm, "R6")
}

#' @rdname rational-functions
#' @export
#' @examples
#' prod(rational(c(2L, 3L), c(5L, 1L), "S7"))
'prod.rational::rationalS7' <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z@n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z@d, USE.NAMES = FALSE)
  .rational_prod(n, d, na.rm, "S7")
}

.rational_sum <- function(n, d, na.rm = FALSE, rational_type = "S4")
{
  d_ <- prod(d, na.rm = na.rm)
  n_ <- sum(n * d_ %/% d, na.rm = na.rm)
  if (n_ > .Machine$integer.max || d_ > .Machine$integer.max)
  {
    warning("Numerator or denominator are too large, converting to numeric")
    return(n_ / d_)
  } else
  {
    g <- .gcd(n_, d_)
    return(rational(as.integer(n_ %/% g), as.integer(d_ %/% g), rational_type))
  }
}

#' @rdname rational-functions
#' @export
#' @examples
#' sum(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("sum", signature = c(x = "rationalS4"),
  function(x, ..., na.rm=FALSE)
  {
    args_ <- list(...)
    if (length(args_) > 0)
    {
      n <- c(x@n, sapply(args_, function(z) z@n, USE.NAMES = FALSE))
      d <- c(x@d, sapply(args_, function(z) z@d, USE.NAMES = FALSE))
    } else
    {
      n <- x@n
      d <- x@d
    }
    .rational_sum(n, d, na.rm, "S4")
  }
)

#' @rdname rational-functions
#' @export
#' @examples
#' sum(rational(c(2L, 3L), c(5L, 1L), "S3"))
sum.rationalS3 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$d, USE.NAMES = FALSE)
  .rational_sum(n, d, na.rm, "S3")
}

#' @rdname rational-functions
#' @export
#' @examples
#' sum(rational(c(2L, 3L), c(5L, 1L), "R6"))
sum.rationalR6 <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z$getNumerator(), USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z$getDenominator(), USE.NAMES = FALSE)
  .rational_sum(n, d, na.rm, "R6")
}

#' @rdname rational-functions
#' @export
#' @examples
#' sum(rational(c(2L, 3L), c(5L, 1L), "S7"))
'sum.rational::rationalS7' <- function(..., na.rm = FALSE)
{
  args_ <- list(...)
  n <- sapply(args_, function(z) z@n, USE.NAMES = FALSE)
  d <- sapply(args_, function(z) z@d, USE.NAMES = FALSE)
  .rational_sum(n, d, na.rm, "S7")
}
