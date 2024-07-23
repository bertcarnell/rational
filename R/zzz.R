# https://rconsortium.github.io/S7/articles/packages.html

.onLoad <- function(...) {
  S7::methods_register()
}
