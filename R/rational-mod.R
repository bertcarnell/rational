# include the rational-class.R so that it is loaded first
#' @include rational-class.R
#' @include rational-add.R
#' @include rational-subtract.R
#' @include rational-integer-division.R

#' @rdname rational-operators
#' @export
#' @examples
#' a <- rational(5L, 1L, "S4")
#' b <- rational(2L, 1L, "S4")
#' d <- rational(3L, 2L, "S4")
#' e <- rational(1L, 2L, "S4")
#' stopifnot(rational(1L, 1L, "S4") == a %% b)
#' stopifnot(rational(0L, 1L, "S4") == d %% e)
#' stopifnot(rational(1L, 1L, "S4") == 5L %% b)
#' stopifnot(rational(1L, 1L, "S4") == a %% 2L)
#' stopifnot(rational(0L, 1L, "S4") == 2L %% e)
#' stopifnot(1 == 5 %% b)
#' stopifnot(1 == a %% 2)
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

#' @rdname rational-operators
#' @export
#' @examples
#' a <- rational(5L, 1L, "S3")
#' b <- rational(2L, 1L, "S3")
#' d <- rational(3L, 2L, "S3")
#' e <- rational(1L, 2L, "S3")
#' stopifnot(rational(1L, 1L, "S3") == a %% b)
#' stopifnot(rational(0L, 1L, "S3") == d %% e)
#' stopifnot(rational(1L, 1L, "S3") == 5L %% b)
#' stopifnot(rational(1L, 1L, "S3") == a %% 2L)
#' stopifnot(rational(0L, 1L, "S3") == 2L %% e)
#' stopifnot(1 == 5 %% b)
#' stopifnot(1 == a %% 2)
'%%.rationalS3' <- function(e1, e2)
{
  e1 - e2 * e1 %/% e2
}

#' @rdname rational-operators
#' @export
#' @examples
#' a <- rational(5L, 1L, "R6")
#' b <- rational(2L, 1L, "R6")
#' d <- rational(3L, 2L, "R6")
#' e <- rational(1L, 2L, "R6")
#' stopifnot(rational(1L, 1L, "R6") == a %% b)
#' stopifnot(rational(0L, 1L, "R6") == d %% e)
#' stopifnot(rational(1L, 1L, "R6") == 5L %% b)
#' stopifnot(rational(1L, 1L, "R6") == a %% 2L)
#' stopifnot(rational(0L, 1L, "R6") == 2L %% e)
#' stopifnot(1 == 5 %% b)
#' stopifnot(1 == a %% 2)
'%%.rationalR6' <- function(e1, e2)
{
  e1 - e2 * e1 %/% e2
}
