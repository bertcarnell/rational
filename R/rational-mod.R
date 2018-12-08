# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include rational-add.R

#' @rdname rational-operators
#' @export
setMethod("%%", signature = c("rationalS4", "rationalS4"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)

#' @rdname rational-operators
#' @export
setMethod("%%", signature = c("integer", "rationalS4"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)

#' @rdname rational-operators
#' @export
setMethod("%%", signature = c("rationalS4", "integer"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)

#' @rdname rational-operators
#' @export
setMethod("%%", signature = c("numeric", "rationalS4"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)

#' @rdname rational-operators
#' @export
setMethod("%%", signature = c("rationalS4", "numeric"),
          function(e1,e2)
          {
            e1 - e2 * e1 %/% e2
          }
)
