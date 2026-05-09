# Null-coalescing helper (base R has no %||% before 4.4).
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Finding constructor and labeler.
#
# Each detector returns a list of "findings": structured records carrying
# the spec schema (section, kind, label, variables, confidence, severity,
# evidence) alongside legacy fields (type, x, y, strength, direction, ...)
# kept for back-compatibility with existing tests.
#
# A finding's `label` is the qualitative sentence rendered by print().
# Detectors should set label up-front via the labelers below; the
# classification layer can override it when kind/strength changes.

.new_finding <- function(section, type, variables,
                         kind        = NA_character_,
                         label       = NA_character_,
                         confidence  = "moderate",
                         severity    = "info",
                         evidence    = list(),
                         ...) {
  c(
    list(
      section    = section,
      type       = type,
      kind       = kind,
      label      = label,
      variables  = variables,
      confidence = confidence,
      severity   = severity,
      evidence   = evidence
    ),
    list(...)
  )
}

# Re-derive a finding's label after classification may have changed kind
# or strength. Idempotent: safe to call repeatedly.
.label_finding <- function(f) {
  f$label <- switch(f$type %||% "",
    numeric_numeric     = .label_numeric_numeric(f),
    categorical_numeric = .label_cat_num(f),
    drift               = .label_drift(f),
    compositional       = .label_compositional(f),
    confounded          = .label_confounded(f),
    multi_target        = .label_multi_target(f),
    nonlinear           = .label_nonlinear(f),
    cat_cat             = .label_cat_cat(f),
    f$label %||% NA_character_
  )
  f
}

.label_numeric_numeric <- function(f) {
  verb <- if ((f$direction %||% "") == "positive") "increases with" else
                                                   "decreases with"
  qual <- switch(f$strength %||% "",
                 strong   = "strongly ",
                 moderate = "",
                 weak     = "weakly ",
                 "")
  adj <- if (isTRUE(f$adjusted)) " (adjusted)" else ""
  sprintf("%s %s%s %s%s", f$y, qual, verb, f$x, adj)
}

.label_cat_num <- function(f) {
  qual <- switch(f$strength %||% "",
                 strong   = "appears to influence",
                 moderate = "appears to influence",
                 weak     = "weakly influences",
                 "is largely independent of")
  sprintf("%s identity %s %s estimates", f$x, qual, f$y)
}

.label_drift <- function(f) {
  qual <- switch(f$strength %||% "",
                 strong   = "changes systematically with",
                 moderate = "drifts with",
                 weak     = "shows weak drift with",
                 "")
  sprintf("%s %s %s", f$y, qual, f$x)
}

.label_compositional <- function(f) {
  sprintf("%s and %s behave as constrained complements", f$x, f$y)
}

.label_confounded <- function(f) {
  sprintf(
    "the relationship between %s and %s weakens after accounting for %s",
    f$y, f$x, paste(f$adjustment_vars, collapse = " and ")
  )
}

.label_multi_target <- function(f) {
  ys <- f$variables[-1L]
  ys_phrase <- if (length(ys) <= 2L) paste(ys, collapse = " and ")
               else paste0(paste(ys[-length(ys)], collapse = ", "),
                           ", and ", ys[length(ys)])
  sprintf("%s appears to structure both %s", f$x, ys_phrase)
}

.label_nonlinear <- function(f) {
  sprintf("%s and %s follow a nonlinear relationship", f$y, f$x)
}

.label_cat_cat <- function(f) {
  qual <- switch(f$strength %||% "",
                 strong   = "is strongly associated with",
                 moderate = "is associated with",
                 weak     = "is weakly associated with",
                 "is largely independent of")
  sprintf("%s %s %s", f$x, qual, f$y)
}

# Re-derive labels for a list of findings. Used after classification.
.relabel_findings <- function(findings) lapply(findings, .label_finding)
