#' Show numeric evidence for screened relationships
#'
#' @param x A `frame_df` object.
#' @param ... Additional arguments (unused).
#' @export
relationships <- function(x, ...) UseMethod("relationships")

#' @param min_strength Minimum strength to display: "negligible", "weak",
#'   "moderate", or "strong".
#' @param type Filter by type: "numeric_numeric", "categorical_numeric", or
#'   NULL (all).
#' @rdname relationships
#' @export
relationships.frame_df <- function(x, min_strength = "weak", type = NULL, ...) {
  strength_rank <- c(negligible = 0L, weak = 1L, moderate = 2L, strong = 3L)
  min_rank      <- strength_rank[[min_strength]]

  findings <- Filter(function(f) {
    strength_rank[[f$strength]] >= min_rank &&
      (is.null(type) || f$type == type)
  }, x$relationship_findings)

  if (length(findings) == 0L) {
    message("No relationships meet the criteria.")
    return(invisible(NULL))
  }

  findings <- findings[order(sapply(findings,
    function(f) -strength_rank[[f$strength]]))]

  cat(sprintf("Relationships  [%d shown]\n\n", length(findings)))

  for (f in findings) {
    ev <- f$evidence
    if (f$type == "numeric_numeric") {
      adj_tag <- if (f$adjusted) "  [adj]" else ""
      cat(sprintf("%s ~ %s\n", f$y, f$x))
      cat(sprintf("  strength: %-10s direction: %s%s\n",
                  f$strength, f$direction, adj_tag))
      cat(sprintf("  r = %6.3f   R² = %.3f   p = %.3g   n = %d\n",
                  ev$r, ev$r2, ev$p, ev$n))
    } else {
      cat(sprintf("%s ↔ %s\n", f$x, f$y))
      cat(sprintf("  strength: %s\n", f$strength))
      cat(sprintf("  η² = %.3f   F = %.3f   p = %.3g   n = %d   groups = %d\n",
                  ev$eta2, ev$f, ev$p, ev$n, ev$k))
    }
    cat("\n")
  }

  invisible(findings)
}
