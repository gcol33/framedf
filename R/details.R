#' Show Methods, Roles, and Skipped Rules
#'
#' `details()` prints how `frame()` did its work: the screening mode it
#' chose, the role assigned to each column, the rules that caused some
#' pairs to be skipped, and which backend was used.
#'
#' This is the place to look when an output from [print.frame_df()] or
#' [relationships()] surprises you and you want to know why a particular
#' pair was or was not screened.
#'
#' @param x A `frame_df` object.
#' @param ... Additional arguments (unused).
#' @return The input invisibly.
#'
#' @examples
#' df <- data.frame(x = 1:30, y = rnorm(30))
#' details(frame(df))
#' @export
details <- function(x, ...) UseMethod("details")

#' @rdname details
#' @export
details.frame_df <- function(x, ...) {
  cat("Details\n\n")

  .section("Analysis mode")
  for (line in .analysis_mode_lines(x)) cat(line, "\n", sep = "")
  cat("\n")

  .section("Column roles")
  for (line in .column_role_lines(x)) cat(line, "\n", sep = "")
  cat("\n")

  .section("Skipped relationship rules")
  for (line in .skipped_rule_lines(x)) cat(line, "\n", sep = "")
  cat("\n")

  .section("Backend")
  for (line in .backend_lines(x)) cat(line, "\n", sep = "")
  cat("\n")

  invisible(x)
}

# ---------------------------------------------------------------------------

.analysis_mode_lines <- function(x) {
  s <- x$settings
  if (x$data_summary$nrow > s$subsample_threshold) {
    c(
      "relationship screening used progressive subsampling",
      "low-association pairs were dropped after a small probe sample",
      "surviving pairs were re-screened on a larger confirmation sample"
    )
  } else {
    c(
      "relationship screening used the full data",
      "all pairs above the minimum complete-case count were screened directly"
    )
  }
}

.column_role_lines <- function(x) {
  roles <- x$roles
  vapply(names(roles), function(nm) {
    sprintf("%s: %s", nm, .role_pretty(roles[[nm]]))
  }, character(1L), USE.NAMES = FALSE)
}

.skipped_rule_lines <- function(x) {
  X <- .SYM_TIMES()
  c(
    sprintf("identifier %s anything: skipped", X),
    sprintf("administrative index %s anything: skipped", X),
    sprintf("near-constant or constant %s anything: skipped", X),
    sprintf("temporal %s measurement: not screened symmetrically; checked as drift instead", X),
    sprintf("coordinate %s measurement: not screened symmetrically; checked as drift instead", X),
    sprintf("many-level grouping %s continuous: screened, but flagged as observer-style if strong", X)
  )
}

.backend_lines <- function(x) {
  use_cpp <- isTRUE(.framedf_env$use_cpp)
  c(
    sprintf("primitives: %s", if (use_cpp) "C++ (Rcpp)" else "R"),
    "numeric numeric pairs: screened by ordinary least squares (with QR residualisation when adjusted)",
    "categorical numeric pairs: one-way analysis-of-variance summaries, eta squared as effect size",
    "compositional pairs: pairwise sum stability under coefficient of variation",
    "drift checks: simple linear fits of spatial coordinates against time"
  )
}
