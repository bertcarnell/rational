#' @include rational-class.R
NULL

#' Extract from a rational vector
#'
#' @param x the rational number
#' @param i index specifying elements
#' @param j index specifying elements
#' @param ... indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL.
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples). This only works for extracting elements, not for the replacement. See drop for further details.
#' @param value the replacement value
#' @param exact controls partial matching when extracting by character
#'
#' @seealso \code{\link{Extract}} for more full descriptions
#'
#' @name rational-extract
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S4")
#'   stopifnot(a[2]@@n == 5L)
#'   stopifnot(all(a[2:3]@@n == c(5,6)))
NULL


#' @rdname rational-extract
#' @export
setMethod("[",
          "rationalS4",
          function(x, i, j, ..., drop)
          {
            rational(x@n[i], x@d[i], "S4")
          })

#' @rdname rational-extract
#' @export
setMethod("[<-",
          "rationalS4",
          function(x, i, j, ..., value)
          {
            x@n[i] <- value@n
            x@d[i] <- value@d
            x@v[i] <- value@v
            return(x)
          })

#' @rdname rational-extract
#' @export
setMethod("[[",
          "rationalS4",
          function(x, i, ..., drop)
          {
            rational(x@n[[i]], x@d[[i]], "S4")
          })

#' @rdname rational-extract
#' @export
setMethod("[[<-",
          "rationalS4",
          function(x, i, ..., value)
          {
            x@n[[i]] <- value@n
            x@d[[i]] <- value@d
            x@v[[i]] <- value@v
            return(x)
          })

#' @rdname rational-extract
#' @export
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "S3")
#'   stopifnot(a[2]$n == 5L)
#'   stopifnot(all(a[2:3]$n == c(5,6)))
'[.rationalS3' <- function(x, i, ..., drop = TRUE)
{
  return(rational(x$n[i], x$d[i], "S3"))
}

#' @rdname rational-extract
#' @export
'[<-.rationalS3' <- function(x, i, ..., value)
{
  if (!is.rationalS3(value))
  {
    stop(.rationalErrorMessage8)
  }
  x$n[i] <- value$n
  x$d[i] <- value$d
  x$v[i] <- value$v
  return(x)
}

#' @rdname rational-extract
#' @export
'[[.rationalS3' <- function(x, i, ..., exact = TRUE)
{
  return(rational(x$n[[i]], x$d[[i]], "S3"))
}

#' @rdname rational-extract
#' @export
'[[<-.rationalS3' <- function(x, i, ..., value)
{
  if (!is.rationalS3(value))
  {
    stop(.rationalErrorMessage8)
  }
  x$n[[i]] <- value$n
  x$d[[i]] <- value$d
  x$v[[i]] <- value$v
  return(x)
}

#' @rdname rational-extract
#' @examples
#'   a <- rational(c(3L, 5L, 6L), c(4L, 5L, 7L), "R6")
#'   stopifnot(a[2]$getNumerator() == 5L)
#'   stopifnot(all(a[2:3]$n == c(5,6)))
#' @export
'[.rationalR6' <- function(x, i, ..., drop = TRUE)
{
  return(rational(x$getNumerator()[i], x$getDenominator()[i], "R6"))
}

#' @rdname rational-extract
#' @export
'[<-.rationalR6' <- function(x, i, ..., value)
{
  if (!is.rationalR6(value))
  {
    stop(.rationalErrorMessage7)
  }
  x$assign_at(i, value)
  return(x)
}

#' @rdname rational-extract
#' @export
'[[.rationalR6' <- function(x, i, ..., exact = TRUE)
{
  return(rational(x$getNumerator()[[i]], x$getDenominator()[[i]], "R6"))
}

#' @rdname rational-extract
#' @export
'[[<-.rationalR6' <- function(x, i, ..., value)
{
  if (!is.rationalR6(value))
  {
    stop(.rationalErrorMessage7)
  }
  x$assign_at(i, value)
  return(x)
}
