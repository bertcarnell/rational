#' Ratinoal Group Generics
#'
#' Group generics allow you to implement methods for many generics at once.
#' You cannot call a group generic directly; instead it is called automatically
#' by members of the group if a more specific method is not found. For example,
#' if you define a method for the `S7_Math` group generic, it will be called
#' when you call `abs()`, `sign()`, `sqrt()`, and many other similar generics
#' (see below for a complete list).
#'
#' @param x,z,e1,e2 Objects used for dispatch.
#' @param ...,na.rm Additional arguments passed to methods.
#' @param .Generic The name of the generic being dispatched on, i.e. if you've
#'   defined a method for `S7_Math` and the user calls `abs()` then `.Generic`
#'   will be `"abs"`.
#'
#'   Use `find_base_generic()` to find the base generic that corresponds to the
#'   generic name.
#' @details
#' # Methods
#'
#' The group generics contain the following methods:
#'
#' * `Ops`: `r paste(group_generics_md("Compare"), group_generics_md("Arith"), group_generics_md("Logic")) `
#' * `Math`: `r group_generics_md("Math")`
#' * `Summary`: `r group_generics_md("Summary")`
#' * `Complex`: `r group_generics_md("Complex")`
#' * `matrixOps`: `r group_generics_md("matrixOps")`
#'
#' @source \url{https://github.com/RConsortium/S7/blob/group-generics/R/method-group.R}
#' @name rational_group_generics
NULL

#' @export
#' @rdname rational_group_generics
S7_Math <- S7::new_generic("Math", "x", function(x, ..., .Generic) {
  S7::S7_dispatch()
})

#' @export
#' @rdname rational_group_generics
S7_Ops <- S7::new_generic("Ops", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

#' @export
#' @rdname rational_group_generics
S7_Compare <- S7::new_generic("S7_Compare", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

#' @export
#' @rdname rational_group_generics
S7_Add <- S7::new_generic("S7_Add", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

S7_Subtract <- S7::new_generic("S7_Subtract", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

S7_Multiply <- S7::new_generic("S7_Multiply", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

S7_Divide <- S7::new_generic("S7_Divide", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

S7_Integer_Divide <- S7::new_generic("S7_Integer_Divide", c("e1", "e2"), function(e1, e2, ..., .Generic) {
  S7::S7_dispatch()
})

#@export
# @rdname S7_group_generics
#S7_Complex <- S7::new_generic("Complex", "z", function(z, ..., .Generic) {
#   S7::S7_dispatch()
# })

# @export
# @rdname S7_group_generics
#S7_Summary <- S7::new_generic("Complex", "z", function(z, ..., .Generic) {
#   S7::S7_dispatch()
# })

#' @export
#' @rdname rational_group_generics
Math.S7_object <- function(x, ...) {
  tryCatch(
    return(S7_Math(x, ..., .Generic = .Generic)),
    S7_error_method_not_found = function(cnd) NULL
  )

  NextMethod()
}

#' @export
#' @rdname rational_group_generics
find_base_generic <- function(.Generic) {
  get(.Generic, mode = "function", envir = baseenv())
}

# for the Markdown file
group_generics_md <- function(name) {
  paste0("`", methods::getGroupMembers(name), "`", collapse = ", ")
}
