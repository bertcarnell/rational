# include the rational-class.R so that it is loaded first
#' @include rational-class.R

#' @title Rational Transformations
#'
#' @param x parameter to be transformed between classes
#' @param ... additional parameters passed to underlying methods
#' @name rational-transformation
NULL

#' @rdname rational-transformation
#' @export
#' @examples
#' as.numeric(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("as.numeric", signature = "rationalS4",
          function(x, ...)
          {
            x@v
          }
)

#' @rdname rational-transformation
#' @export
#' @examples
#' as.integer(rational(c(2L, 3L), c(5L, 1L), "S4"))
#' as.integer(rational(8L, 3L, "S4"))
setMethod("as.integer", signature = "rationalS4",
          function(x, ...)
          {
            if (length(x) == 1)
            {
              if (x@d == 1)
              {
                as.integer(x@n)
              } else
              {
                as.integer(x@v)
              }
            } else
            {
              ind <- which(x@d == 1)
              ret <- as.integer(x@v)
              ret[ind] <- as.integer(x@n[ind])
              return(ret)
            }
          }
)

#' @rdname rational-transformation
#' @export
#' @examples
#' as.character(rational(c(2L, 3L), c(5L, 1L), "S4"))
setMethod("as.character", signature = "rationalS4",
          function(x, ...)
          {
            paste(x@n, "/", x@d)
          }
)

#' @rdname rational-transformation
#' @export
setGeneric("as.rationalS4",
           def = function(x, cycles = 10, max.denominator = 2000)
           {
             standardGeneric("as.rationalS4")
           }
)

#' @rdname rational-transformation
#' @param cycles The maximum number of steps to be used in the continued fraction approximation process
#' @param max.denominator If the denominator exceeds this number, the algorithm will stop with an approximation
#' @export
#' @examples
#' as.rationalS4(33.3)
setMethod("as.rationalS4", signature = "numeric",
          function(x, cycles=10, max.denominator=2000)
          {
            if (!is.integer(x))
            {
              r <- .rat(x, cycles, max.denominator)$rat
              if (length(x) == 1)
              {
                if (abs(r[1] / r[2] - x) > 1E-9)
                {
                  warning("as.rational produced an approximate rational number")
                }
                structure(.Data = rational(as.integer(r[1]), as.integer(r[2]), "S4"),
                          abs.error = abs(r[1] / r[2] - x),
                          class = c("rationalS4", "numeric"))
              } else if (length(x) > 1)
              {
                if (any(abs(r[,1] / r[,2] - x) > 1E-9))
                {
                  warning("as.rational produced an approximate rational number")
                }
                structure(.Data = rational(as.integer(r[,1]), as.integer(r[,2]), "S4"),
                          abs.error = abs(r[,1] / r[,2] - x),
                          class = c("rationalS4", "numeric"))
              }
            } else
            {
              rational(as.integer(x), 1L, "S4")
            }
          }
)

#' @rdname rational-transformation
#' @export
setMethod("as.rationalS4", signature = "character",
          function(x, cycles = 10, max.denominator = 2000)
          {
            b <- as.numeric(x)
            as.rationalS4(b, cycles, max.denominator)
          }
)
