# include the rational-class.R so that it is loaded first
#' @include rational-class.R

#' @title Is an object from class rational
#'
#' Test if an object is of class "rationalS3", "rationalS4", or "rationalR6"
#'
#' @note The \code{inherit} parameter is included for speed.  There is a
#' performance boost from not checking for inheritence in the class structure.
#' When \code{inherit==FALSE}, the the first class returned from \code{class()} is
#' checked to be equal to "rationalXX".  When \code{inherit==TRUE}, then the
#' \code{methods::is()} function is used.

#' @param e1 object to be tested
#' @param inherit should inheritence be checked? default=\code{FALSE}
#'
#' @name isRational
NULL

#' @rdname isRational
is.rationalS3 <- function(e1, inherit=FALSE)
{
  if (inherit) return(is(e1, "rationalS3"))
  else return(class(e1)[1] == "rationalS3")
}

#' @rdname isRational
is.rationalR6 <- function(e1, inherit=FALSE)
{
  if (inherit) return(is(e1, "rationalR6"))
  else return(class(e1)[1] == "rationalR6")
}

#' @rdname isRational
is.rationalS4 <- function(e1, inherit=FALSE)
{
  if (inherit) return(is(e1, "rationalS4"))
  else return(class(e1)[1] == "rationalS4")
}

#' @rdname isRational
is.rational <- function(e1, inherit=FALSE)
{
  if (inherit) return(is(e1, "rationalS4") || is(e1, "rationalS3") || is(e1, "rationalR6"))
  else return(any(class(e1) %in% c("rationalS4", "rationalS3", "rationalR6")))
}
