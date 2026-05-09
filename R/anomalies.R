#' Show anomaly evidence
#'
#' @param x A `frame_df` object.
#' @param ... Additional arguments (unused).
#' @export
anomalies <- function(x, ...) UseMethod("anomalies")

#' @rdname anomalies
#' @export
anomalies.frame_df <- function(x, ...) {
  findings <- x$anomaly_findings

  if (length(findings) == 0L) {
    message("No anomalies found.")
    return(invisible(NULL))
  }

  cat(sprintf("Anomalies  [%d findings]\n\n", length(findings)))

  for (f in findings) {
    if (f$type == "outlier") {
      cat(sprintf("%s: %d outliers  (method: %s)\n",
                  f$column, f$n, f$method))
      cat(sprintf("  fence: [%.4g, %.4g]\n", f$bounds[[1]], f$bounds[[2]]))
      idx_show <- head(f$indices, 10L)
      suffix   <- if (length(f$indices) > 10L)
        sprintf(" … +%d more", length(f$indices) - 10L) else ""
      cat(sprintf("  rows: %s%s\n", paste(idx_show, collapse = ", "), suffix))
    } else if (f$type == "skewness") {
      cat(sprintf("%s: %s-skewed  (skewness = %.3f, method: %s)\n",
                  f$column, f$direction, f$skewness, f$method))
    }
    cat("\n")
  }

  invisible(findings)
}
