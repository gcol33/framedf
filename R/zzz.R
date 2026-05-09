# Package-level state: whether C++ primitives compiled and loaded.
.framedf_env <- new.env(parent = emptyenv())
.framedf_env$use_cpp <- FALSE

.onLoad <- function(libname, pkgname) {
  # The Rcpp-generated entry point is registered as "_framedf_lm_simple_cpp".
  .framedf_env$use_cpp <- is.loaded("_framedf_lm_simple_cpp")
}
