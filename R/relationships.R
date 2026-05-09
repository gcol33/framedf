#' Show Relationships Found by `frame()`
#'
#' Prints a structured listing of all relationship findings, grouped into
#' four kinds: meaningful, suspicious, structural, and ignored. Each pair
#' is described qualitatively (direction, strength, stability, method).
#'
#' Numeric evidence (correlation coefficients, F-statistics, p-values) is
#' available on the returned object; the printed view is meant to be
#' skimmable.
#'
#' @param x A `frame_df` object.
#' @param ... Additional arguments (unused).
#' @return The list of findings, invisibly.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100)
#' )
#' df$z <- 0.8 * df$x + 0.2 * stats::rnorm(100)
#' relationships(frame(df))
#' @export
relationships <- function(x, ...) UseMethod("relationships")

#' @param min_strength Minimum strength to display: `"weak"`, `"moderate"`,
#'   or `"strong"`. Structural relationships are always shown.
#' @param kind Restrict to a single kind: `"meaningful"`, `"suspicious"`,
#'   `"structural"`, `"ignored"`, or `NULL` (all).
#' @rdname relationships
#' @export
relationships.frame_df <- function(x, min_strength = "weak",
                                   kind = NULL, ...) {
  rank <- c(negligible = 0L, weak = 1L, moderate = 2L, strong = 3L,
            structural = 3L)
  cutoff <- rank[[min_strength]]

  findings <- Filter(function(f) {
    s <- rank[[f$strength]]
    if (is.null(s)) s <- 0L
    s >= cutoff && (is.null(kind) || (f$kind %||% "") == kind)
  }, x$relationship_findings)

  cat("Relationships\n\n")

  any_shown <- FALSE
  for (k in c("meaningful", "suspicious", "structural")) {
    if (!is.null(kind) && kind != k) next
    block <- Filter(function(f) (f$kind %||% "") == k, findings)
    if (length(block) == 0L) next
    any_shown <- TRUE
    .section(k)
    for (f in block) {
      .print_relationship_block(f)
    }
  }

  if (is.null(kind) || kind == "ignored") {
    if (length(x$ignored_cols) > 0L) {
      any_shown <- TRUE
      .section("ignored")
      .print_ignored_pairs(x)
    }
  }

  if (!any_shown) {
    message("No relationships meet the criteria.")
  }

  invisible(findings)
}

# Each pair gets a short structured block:
#   y ~ x
#     direction: positive
#     strength:  strong
#     stability: high
#     method:    numeric screening
.print_relationship_block <- function(f) {
  pair <- sprintf("%s ~ %s", f$y, f$x)
  cat(pair, "\n", sep = "")

  fields <- list()
  if (f$type == "numeric_numeric") {
    fields$direction <- f$direction
    fields$strength  <- f$strength
    fields$stability <- f$stability %||% "high"
    fields$method    <- "numeric screening"
    if (isTRUE(f$adjusted)) fields$method <- "numeric screening (adjusted)"
  } else if (f$type == "categorical_numeric") {
    fields$pattern   <- "group effect"
    fields$strength  <- f$strength
    fields$stability <- f$stability %||% "high"
    fields$method    <- "categorical numeric screen"
    if (!is.na(f$concern %||% NA_character_)) fields$concern <- f$concern
  } else if (f$type == "drift") {
    fields$pattern   <- "temporal spatial drift"
    fields$strength  <- f$strength
    fields$stability <- f$stability %||% "high"
    fields$method    <- "drift screen"
    if (!is.na(f$concern %||% NA_character_)) fields$concern <- f$concern
  } else if (f$type == "compositional") {
    fields$pattern   <- "constrained complement"
    fields$concern   <- f$concern %||% "compositional relationship"
    fields$method    <- "compositional sum check"
  }

  for (nm in names(fields)) {
    cat(sprintf("  %s: %s\n", nm, fields[[nm]]))
  }
  cat("\n")
}

.print_ignored_pairs <- function(x) {
  roles <- x$roles
  for (nm in x$ignored_cols) {
    reason <- .role_ignore_reason(roles[[nm]])
    if (is.null(reason)) reason <- paste0("role = ", roles[[nm]])
    # Pick a representative measurement column to make the pair concrete,
    # mirroring the example output style ("X ~ measurement").
    target <- .pick_target_for(x)
    if (is.null(target)) {
      cat(sprintf("%s\n  reason: %s\n\n", nm, reason))
    } else {
      cat(sprintf("%s ~ %s\n  reason: %s\n\n", nm, target, reason))
    }
  }
}

.pick_target_for <- function(x) {
  # Pick the first measurement column as a generic stand-in.
  meas <- names(x$roles)[x$roles %in% c("measurement", "compositional")]
  if (length(meas) == 0L) return(NULL)
  meas[[1]]
}
