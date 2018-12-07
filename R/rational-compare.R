#' @rdname rational-class
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

#' @rdname rational-class
setMethod("Compare", signature = c("rationalS4", "integer"),
          function(e1, e2)
          {
            callGeneric(e1@n, e2*e1@d)
          }
)

#' @rdname rational-class
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

#' @rdname rational-class
setMethod("Compare", signature = c("integer", "rationalS4"),
          function(e1, e2)
          {
            callGeneric(e1*e2@d, e2@n)
          }
)

#' @rdname rational-class
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
