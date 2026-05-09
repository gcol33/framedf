#' Show Anomaly Findings from `frame()`
#'
#' Lists per-column anomalies grouped by qualitative pattern: implausible
#' values, totals exceeding bounds, capitalization inconsistency, rare
#' levels, distributional outliers, and skewed distributions.
#'
#' Numeric backing detail (Tukey fences, skewness coefficients, indices of
#' offending rows) is available on the returned object; the print view is
#' meant to be skimmable.
#'
#' @param x A `frame_df` object.
#' @param ... Additional arguments (unused).
#' @return The list of anomaly findings, invisibly.
#'
#' @examples
#' df <- data.frame(
#'   y = c(rnorm(98), 100, -100),
#'   g = c(rep("A", 49), rep("a", 49), "B", "B")
#' )
#' anomalies(frame(df))
#' @export
anomalies <- function(x, ...) UseMethod("anomalies")

#' @rdname anomalies
#' @export
anomalies.frame_df <- function(x, ...) {
  findings <- x$anomaly_findings

  cat("Anomalies\n\n")
  if (length(findings) == 0L) {
    cat("No anomalies detected by the default checks.\n")
    return(invisible(NULL))
  }

  grouped <- split(findings, vapply(findings, function(f) f$tag, character(1L)))

  for (tag in names(grouped)) {
    .section(tag)
    for (item in grouped[[tag]]) {
      cat(.anomaly_block(item), sep = "")
    }
  }

  invisible(findings)
}

# Multi-line block for a single finding. Returns a single character vector
# already terminated by a newline.
.anomaly_block <- function(f) {
  switch(f$type,
    outlier = sprintf(
      "%s\n  pattern: outliers (Tukey fence)\n  count: %d\n  fence: [%g, %g]\n\n",
      f$column, f$n, f$bounds[[1]], f$bounds[[2]]
    ),
    skewness = sprintf(
      "%s\n  pattern: %s-skewed distribution\n  skewness: %.3f\n\n",
      f$column, f$direction, f$skewness
    ),
    implausible_range = sprintf(
      "%s\n  pattern: implausible values\n  expected: [%g, %g]\n  observed: [%g, %g]\n\n",
      f$column, f$bounds[[1]], f$bounds[[2]], f$observed[[1]], f$observed[[2]]
    ),
    out_of_bounds = sprintf(
      "%s\n  pattern: %s expected total\n  count: %d\n  scale: %g\n\n",
      f$column,
      if (f$direction == "above") "exceeds" else "below",
      f$n, f$scale
    ),
    pair_exceeds_total = sprintf(
      "%s\n  pattern: pair exceeds expected total\n  count: %d\n  columns: %s\n\n",
      f$column, f$n, paste(f$cols, collapse = ", ")
    ),
    case_inconsistency = sprintf(
      "%s\n  pattern: inconsistent capitalization\n  examples: %s\n\n",
      f$column, paste(f$examples, collapse = ", ")
    ),
    rare_levels = sprintf(
      "%s\n  pattern: levels represented by very few rows\n  count: %d levels < %d rows\n  examples: %s\n\n",
      f$column, f$n_rare, f$threshold, paste(f$examples, collapse = ", ")
    ),
    sprintf("%s\n  type: %s\n\n", f$column, f$type)
  )
}
