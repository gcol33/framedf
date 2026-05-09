# Symbol constants used in the qualitative output. We build them from
# Unicode codepoints so this source file stays pure ASCII (CRAN policy).
.SYM_TIMES   <- function() intToUtf8(0x00D7)  # multiplication sign
.SYM_HRULE16 <- function() strrep(intToUtf8(0x2500), 16L)  # box drawing
.SYM_BULLET  <- function() intToUtf8(0x2022)  # bullet

#' Print a `frame_df` Object
#'
#' Prints a qualitative narrative of the data frame. The output is
#' organised into four sections: Structure, Relationships, Anomalies, and
#' Ignored. No raw correlation values, p-values, or test statistics are
#' shown; those live in [relationships()] and [details()] for readers
#' who want them.
#'
#' @param x A `frame_df` object.
#' @param ... Unused.
#' @return The input invisibly.
#' @export
print.frame_df <- function(x, ...) {
  cat("framedf\n\n", sep = "")
  cat(sprintf("%s rows %s %s columns\n\n",
              .fmt_int(x$data_summary$nrow),
              .SYM_TIMES(),
              .fmt_int(x$data_summary$ncol)), sep = "")

  .print_structure_section(x)
  .print_relationships_section(x)
  .print_anomalies_section(x)
  .print_missingness_section(x)
  .print_inflation_section(x)
  .print_ignored_section(x)

  invisible(x)
}

.print_missingness_section <- function(x) {
  findings <- x$missingness_findings %||% list()
  if (length(findings) == 0L) return(invisible(NULL))
  .section("Missingness")
  for (f in findings) cat(f$label, "\n", sep = "")
  cat("\n")
}

.print_inflation_section <- function(x) {
  findings <- x$inflation_findings %||% list()
  if (length(findings) == 0L) return(invisible(NULL))
  .section("Inflation and sparsity")
  for (f in findings) cat(f$label, "\n", sep = "")
  cat("\n")
}

# ---------------------------------------------------------------------------
# Section: Structure
# ---------------------------------------------------------------------------

.print_structure_section <- function(x) {
  s <- x$structure
  .section("Structure")
  cat(.shape_phrase(s$shape), "\n\n", sep = "")

  if (!is.null(s$observation_unit)) {
    .bullet_block("Detected observation unit:", s$observation_unit)
  }

  .bullet_block("Detected temporal structure:", s$temporal)
  .bullet_block("Detected spatial structure:",  s$spatial)
  ids <- c(s$identifiers, s$group_ids)
  .bullet_block("Likely identifiers:",          ids)

  group_lines <- character(0)
  for (g in s$repeated_within) {
    group_lines <- c(group_lines, sprintf("repeated observations within %s", g))
  }
  for (gc in s$grouping_cats) {
    group_lines <- c(group_lines, sprintf("observations grouped by %s", gc))
  }
  .bullet_block("Possible grouping structure:", group_lines)

  if (length(s$nested) > 0L) {
    nested_lines <- vapply(s$nested, function(p) {
      sprintf("%s nested within %s", p$child, p$parent)
    }, character(1L))
    .bullet_block("Possible nested structure:", nested_lines)
  }

  if (length(s$compositional_cols) > 0L) {
    .bullet_block("Possible compositional structure:", s$compositional_cols)
  }

  cat("\n")
}

# ---------------------------------------------------------------------------
# Section: Relationships
# ---------------------------------------------------------------------------

.print_relationships_section <- function(x) {
  findings <- x$relationship_findings
  shown    <- Filter(function(f) (f$kind %||% "") %in%
                       c("meaningful", "suspicious", "structural"), findings)

  .section("Relationships")
  if (length(shown) == 0L) {
    cat("Nothing notable above the configured strength thresholds.\n\n")
    return(invisible(NULL))
  }

  for (kind in c("meaningful", "suspicious", "structural")) {
    block <- Filter(function(f) (f$kind %||% "") == kind, shown)
    for (f in block) cat(.narrative_line(f), "\n\n", sep = "")
  }
}

# Render a single finding as a one- or two-line natural-language sentence.
# Prefers the precomputed `label` from the labeler; falls back to a
# type-specific narrative for safety.
.narrative_line <- function(f) {
  base <- if (!is.null(f$label) && !is.na(f$label)) {
    f$label
  } else {
    switch(f$type %||% "",
      numeric_numeric     = .narrative_numeric(f),
      categorical_numeric = .narrative_cat(f),
      drift               = .narrative_drift(f),
      compositional       = .narrative_comp(f),
      sprintf("%s ~ %s", f$y, f$x)
    )
  }
  concern <- f$concern %||% NA_character_
  if (!is.na(concern) && (f$kind %||% "") == "suspicious") {
    base <- paste0(base, "\n  ", concern)
  }
  base
}

.narrative_numeric <- function(f) {
  verb <- if (f$direction == "positive") "increases with" else "decreases with"
  qual <- switch(f$strength,
                 strong   = "strongly ",
                 moderate = "",
                 weak     = "weakly ",
                 "")
  adj  <- if (isTRUE(f$adjusted)) " (adjusted)" else ""
  sprintf("%s %s%s %s%s", f$y, qual, verb, f$x, adj)
}

.narrative_cat <- function(f) {
  qual <- switch(f$strength,
                 strong   = "appears to influence",
                 moderate = "appears to influence",
                 weak     = "weakly influences",
                 "is largely independent of")
  sprintf("%s identity %s %s estimates", f$x, qual, f$y)
}

.narrative_drift <- function(f) {
  qual <- switch(f$strength,
                 strong   = "changes systematically with",
                 moderate = "drifts with",
                 weak     = "shows weak drift with",
                 "")
  sprintf("%s %s %s", f$y, qual, f$x)
}

.narrative_comp <- function(f) {
  sprintf("%s and %s behave as constrained complements", f$x, f$y)
}

# ---------------------------------------------------------------------------
# Section: Anomalies
# ---------------------------------------------------------------------------

.print_anomalies_section <- function(x) {
  findings <- x$anomaly_findings
  .section("Anomalies")
  if (length(findings) == 0L) {
    cat("No anomalies detected by the default checks.\n\n")
    return(invisible(NULL))
  }

  # Group by tag so each tag prints one line per column it touches,
  # instead of repeating "outliers" five times in a row.
  grouped <- split(findings, vapply(findings, function(f) f$tag, character(1L)))

  for (tag in names(grouped)) {
    cat(.anomaly_phrase(tag, grouped[[tag]]), "\n", sep = "")
  }
  cat("\n", sep = "")
}

.anomaly_phrase <- function(tag, items) {
  cols <- vapply(items, function(it) it$column, character(1L))
  cols <- unique(cols)
  one  <- length(cols) == 1L
  noun <- if (one) cols[[1]] else .humanise_columns(cols)

  switch(tag,
    implausible_values =
      sprintf("%s contain%s implausible values", noun,
              if (one) "s" else ""),
    exceed_expected_totals =
      sprintf("%s occasionally exceed expected totals", noun),
    inconsistent_capitalization =
      sprintf("%s contain%s inconsistent capitalization", noun,
              if (one) "s" else ""),
    very_few_rows_levels =
      sprintf("%s have categorical levels represented by very few rows", noun),
    outliers =
      sprintf("%s contain%s extreme values relative to most observations",
              noun, if (one) "s" else ""),
    skewed_distribution =
      sprintf("%s show%s a strongly skewed distribution", noun,
              if (one) "s" else ""),
    isolated_temporal =
      sprintf("%s has isolated values far outside the main sampling period",
              noun),
    possible_coord_swap = {
      cols <- items[[1L]]$cols
      sprintf("some %s and %s values may be swapped", cols[[1L]], cols[[2L]])
    },
    sprintf("%s flagged: %s", noun, tag)
  )
}

# ---------------------------------------------------------------------------
# Section: Ignored
# ---------------------------------------------------------------------------

.print_ignored_section <- function(x) {
  cols <- x$ignored_cols
  if (length(cols) == 0L) return(invisible(NULL))
  .section("Ignored relationships")
  roles <- x$roles
  for (nm in cols) {
    reason <- .role_ignore_reason(roles[[nm]])
    if (is.null(reason)) reason <- paste0("role = ", roles[[nm]])
    cat(sprintf("%s was ignored because %s\n", nm, reason))
  }
  cat("\n")
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.section <- function(title) {
  cat(title, "\n", sep = "")
  cat(.SYM_HRULE16(), "\n", sep = "")
}

.bullet_block <- function(header, items) {
  if (length(items) == 0L) return(invisible(NULL))
  cat(header, "\n", sep = "")
  for (it in items) cat(.SYM_BULLET(), " ", it, "\n", sep = "")
  cat("\n")
}

.humanise_columns <- function(cols) {
  if (length(cols) == 1L) return(cols)
  if (length(cols) == 2L) return(paste(cols, collapse = " and "))
  paste0(paste(cols[-length(cols)], collapse = ", "), ", and ", cols[length(cols)])
}

.fmt_int <- function(n) formatC(n, big.mark = ",", format = "d")
