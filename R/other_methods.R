
.rationalIntDivRational <- function(n1, d1, n2, d2) (n1 * d2) %/% (n2 * d1)
.integerIntDivRational <- function(i1, n2, d2) (i1 * d2) %/% n2
.rationalIntDivInteger <- function(n1, d1, i2) n1 %/% (i2 * d1)
.rationalIntDivNumeric <- function(v1, v2) v1 %/% v2
.numericIntDivRational <- function(v1, v2) v1 %/% v2


setMethod("%/%", signature = c("rationalS4", "rationalS4"),
  function(e1,e2)
  {
    .rationalIntDivRational(e1@n, e1@d, e2@n, e2@d)
  }
)
setMethod("%/%", signature = c("integer", "rationalS4"),
          function(e1,e2)
          {
            .integerIntDivRational(e1, e2@n, e2@d)
          }
)
setMethod("%/%", signature = c("rationalS4", "integer"),
          function(e1,e2)
          {
            .rationalIntDivInteger(e1@n, e1@d, e2)
          }
)
setMethod("%/%", signature = c("numeric", "rationalS4"),
          function(e1,e2)
          {
            .numericIntDivRational(e1, e@v)
          }
)
setMethod("%/%", signature = c("rationalS4", "numeric"),
          function(e1,e2)
          {
            .rationalIntDivNumeric(e1@v, e2)
          }
)

setMethod("%%", signature = c("rationalS4", "rationalS4"),
  function(e1,e2)
  {
    e1 - e2 * e1 %/% e2
  }
)
setMethod("%%", signature = c("integer", "rationalS4"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)
setMethod("%%", signature = c("rationalS4", "integer"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)
setMethod("%%", signature = c("numeric", "rationalS4"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)
setMethod("%%", signature = c("rationalS4", "numeric"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)


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

setMethod("Compare", signature = c("rationalS4", "integer"),
  function(e1, e2)
  {
    callGeneric(e1@n, e2*e1@d)
  }
)

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

setMethod("Compare", signature = c("integer", "rationalS4"),
  function(e1, e2)
  {
    callGeneric(e1*e2@d, e2@n)
  }
)

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

setMethod("Math", signature = c("rationalS4"),
  function(x)
  {
    callGeneric(x@v)
  }
)

setMethod("Math2", signature = c("rationalS4"),
  function(x)
  {
    callGeneric(x@v)
  }
)

setMethod("abs", signature = c("rationalS4"),
  function(x)
  {
    n <- abs(x@n)
    d <- abs(x@d)
    rational(n, d, "S4")
  }
)

setMethod("log", signature = c("rationalS4"),
  function(x)
  {
    log(x@n) - log(x@d)
  }
)

setMethod("log10", signature = c("rationalS4"),
  function(x)
  {
    log10(x@n) - log10(x@d)
  }
)

setGeneric("logb")

setMethod("logb", signature = c("rationalS4"),
  function(x, base)
  {
    logb(x@n, base = base) - logb(x@d, base = base)
  }
)

setMethod("log2", signature = c("rationalS4"),
  function(x)
  {
    log2(x@n) - log2(x@d)
  }
)

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

setMethod("max", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    m <- which.max(x@v)
    rational(x@n[m], x@d[m], "S4")
  }
)

setMethod("min", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    m <- which.min(x@v)
    rational(x@n[m], x@d[m], "S4")
  }
)

setMethod("range", signature = c("rationalS4"),
  function(x, na.rm=FALSE)
  {
    c(min(x), max(x))
  }
)

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

setMethod("as.numeric", signature = "rationalS4",
  function(x,...)
  {
    x@v
  }
)

setMethod("as.integer", signature = "rationalS4",
  function(x,...)
  {
    if (x@d == 1)
    {
      as.integer(x@n)
    } else
    {
      as.integer(x@v)
    }
  }
)

setMethod("as.character", signature = "rationalS4",
  function(x,...)
  {
    paste(x@n, "/", x@d)
  }
)

setGeneric("as.rationalS4",
  def = function(x, cycles = 10, max.denominator = 2000)
  {
    standardGeneric("as.rationalS4")
  }
)

setMethod("as.rationalS4", signature = "numeric",
  function(x, cycles=10, max.denominator=2000)
  {
    if (!is.integer(x))
    {
      r <- MASS:::.rat(x, cycles, max.denominator)$rat
      if (length(x) == 1)
      {
        if (r[1] / r[2] != x)
        {
          warning("as.rational produced an approximate rational number")
        }
        structure(.Data = rational(as.integer(r[1]), as.integer(r[2]), "S4"),
                  abs.error = abs(r[1] / r[2] - x),
                  class = c("rational", "numeric"))
      } else if (length(x) > 1)
      {
        if (any(r[,1] / r[,2] != x))
        {
          warning("as.rational produced an approximate rational number")
        }
        structure(.Data = rational(as.integer(r[,1]), as.integer(r[,2]), "S4"),
                  abs.error = abs(r[,1] / r[,2] - x),
                  class = c("rational", "numeric"))
      }
    } else
    {
      rational(as.integer(x), 1L, "S4")
    }
  }
)

setMethod("as.rationalS4", signature = "character",
  function(x, cycles = 10, max.denominator = 2000)
  {
    b <- as.numeric(x)
    as.rationalS4(b, cycles, max.denominator)
  }
)
