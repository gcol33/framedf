#' @export
print.frame_df <- function(x, ...) {
  cat(sprintf("frame_df  [%d × %d]\n\n",
              x$data_summary$nrow, x$data_summary$ncol))

  .print_roles_section(x)
  .print_relationships_section(x)
  .print_anomalies_section(x)
  .print_ignored_section(x)

  cat("Use relationships(x), anomalies(x), details(x) for more.\n")
  invisible(x)
}

.print_roles_section <- function(x) {
  roles <- x$roles
  groups <- list(
    measurement  = names(roles[roles == "measurement"]),
    categorical  = names(roles[roles %in% c("categorical", "flag")]),
    identifier   = names(roles[roles %in% c("id", "admin_index")]),
    date         = names(roles[roles == "date"]),
    coordinate   = names(roles[grepl("^coord_", roles)]),
    near_constant = names(roles[roles %in% c("constant", "near_constant")])
  )
  if (all(lengths(groups) == 0L)) return(invisible(NULL))

  cat("Roles\n")
  for (grp in names(groups)) {
    cols <- groups[[grp]]
    if (length(cols) == 0L) next
    cat(sprintf("  %-14s(%d):  %s\n", grp, length(cols),
                paste(cols, collapse = "  ")))
  }
  cat("\n")
}

.print_relationships_section <- function(x) {
  findings   <- x$relationship_findings
  n_screened <- x$n_pairs_screened

  cat(sprintf("Relationships  [%d pairs screened]\n", n_screened))

  visible <- Filter(function(f) f$strength != "negligible", findings)
  if (length(visible) == 0L) {
    cat("  none above threshold\n\n")
    return(invisible(NULL))
  }

  strength_order <- c(strong = 1L, moderate = 2L, weak = 3L, negligible = 4L)
  visible <- visible[order(sapply(visible, function(f) strength_order[f$strength]))]

  for (f in visible) {
    label  <- tools::toTitleCase(f$strength)
    if (f$type == "numeric_numeric") {
      rel    <- sprintf("%s ~ %s", f$y, f$x)
      detail <- paste(f$shape, f$direction, sep = ", ")
      if (f$adjusted) detail <- paste0(detail, " [adj]")
    } else {
      rel    <- sprintf("%s ↔ %s", f$x, f$y)
      detail <- "group difference"
    }
    cat(sprintf("  %-9s %-40s %s\n", label, rel, detail))
  }
  cat("\n")
}

.print_anomalies_section <- function(x) {
  findings <- x$anomaly_findings
  if (length(findings) == 0L) {
    cat("Anomalies  [none]\n\n")
    return(invisible(NULL))
  }

  cat(sprintf("Anomalies  [%d found]\n", length(findings)))
  by_col <- split(findings, sapply(findings, `[[`, "column"))

  for (col_nm in names(by_col)) {
    descs <- sapply(by_col[[col_nm]], function(f) {
      if (f$type == "outlier")  sprintf("%d outliers (Tukey)", f$n)
      else if (f$type == "skewness") sprintf("%s-skewed", f$direction)
      else f$type
    })
    cat(sprintf("  %s: %s\n", col_nm, paste(descs, collapse = ", ")))
  }
  cat("\n")
}

.print_ignored_section <- function(x) {
  cols <- x$ignored_cols
  if (length(cols) == 0L) return(invisible(NULL))

  roles   <- x$roles
  reasons <- vapply(cols, function(nm) {
    r <- .role_ignore_reason(roles[[nm]])
    if (is.null(r)) paste0("role = ", roles[[nm]]) else r
  }, character(1L))

  cat(sprintf("Ignored  [%d]\n", length(cols)))
  for (i in seq_along(cols)) {
    cat(sprintf("  %s was ignored because %s\n", cols[[i]], reasons[[i]]))
  }
  cat("\n")
}
