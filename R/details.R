#' Show methods, thresholds, sample sizes, and skipped rules
#'
#' @param x A `frame_df` object.
#' @param ... Additional arguments (unused).
#' @export
details <- function(x, ...) UseMethod("details")

#' @rdname details
#' @export
details.frame_df <- function(x, ...) {
  cat("frame_df details\n\n")

  # Data dimensions
  cat(sprintf("Data:  %d rows × %d columns\n\n",
              x$data_summary$nrow, x$data_summary$ncol))

  # Backend
  use_cpp <- isTRUE(.framedf_env$use_cpp)
  cat(sprintf("Backend: %s\n\n", if (use_cpp) "C++ (Rcpp)" else "R prototype"))

  # Thresholds
  s <- x$settings
  cat("Thresholds\n")
  cat(sprintf("  strong   ≥ %.2f\n", s$strong_threshold))
  cat(sprintf("  moderate ≥ %.2f\n", s$moderate_threshold))
  cat(sprintf("  weak     ≥ %.2f\n", s$weak_threshold))
  cat(sprintf("  min_obs    %d\n",   s$min_obs))
  if (!is.null(s$adjustment)) {
    cat(sprintf("  adjustment: %s\n", paste(s$adjustment, collapse = ", ")))
  }
  cat("\n")

  # Methods
  cat("Methods\n")
  cat("  numeric-numeric:    Pearson r via OLS (with QR residualisation if adjusted)\n")
  cat("  categorical-numeric: one-way ANOVA, η²\n")
  cat("  outlier detection:  Tukey IQR fence\n")
  cat("  skewness:           moment-based (m3 / m2^(3/2))\n")
  cat("\n")

  # Column fingerprints
  cat("Columns\n")
  fps <- x$column_fingerprints
  for (nm in names(fps)) {
    fp   <- fps[[nm]]
    role <- x$roles[[nm]]
    n_tot <- fp$n_valid + fp$n_miss
    miss_pct <- if (n_tot > 0) 100 * fp$n_miss / n_tot else 0
    line <- sprintf("  %-22s %-14s role=%-14s n_valid=%-6d miss=%.1f%%",
                    nm, paste(fp$class, collapse = "/"), role,
                    fp$n_valid, miss_pct)
    if (!is.null(fp$mean)) {
      line <- paste0(line, sprintf("  mean=%-.4g  sd=%-.4g", fp$mean, fp$sd))
    } else {
      line <- paste0(line, sprintf("  n_unique=%d", fp$n_unique))
    }
    cat(line, "\n")
  }
  cat("\n")

  # Ignored columns
  if (length(x$ignored_cols) > 0L) {
    cat("Ignored columns\n")
    roles <- x$roles
    for (nm in x$ignored_cols) {
      reason <- .role_ignore_reason(roles[[nm]])
      if (is.null(reason)) reason <- paste0("role = ", roles[[nm]])
      cat(sprintf("  %s was ignored because %s\n", nm, reason))
    }
    cat("\n")
  }

  # Skipped pairs (insufficient obs)
  ignored_pairs <- x$ignored_pairs
  if (length(ignored_pairs) > 0L) {
    reason_tbl <- table(sapply(ignored_pairs, `[[`, "reason"))
    cat("Skipped pairs\n")
    for (r in names(reason_tbl)) {
      cat(sprintf("  %s: %d pair(s)\n", r, reason_tbl[[r]]))
    }
    cat("\n")
  }

  invisible(x)
}
