# Inflation and sparsity detection.
#
# Flags four patterns:
#   - zero-inflation: a numeric column with a large mass at zero on top
#     of a continuous distribution (zero_fraction >= 0.3)
#   - dominant extreme values: a small set of values accounting for most
#     of the mass (dominant_freq >= 0.5 on a non-near-constant column)
#   - discretization: a numeric column with very few unique values
#     relative to its sample size, or where most values are integers
#   - singleton-heavy categorical: many levels appearing exactly once
#
# These complement classic anomalies (outliers, skew) by describing the
# *shape* of a column rather than individual offending rows.

.detect_inflation <- function(df, roles, fps, settings) {
  out <- list()

  for (nm in names(df)) {
    role <- roles[[nm]]
    fp   <- fps[[nm]]

    if (role %in% c("measurement", "compositional") &&
        isTRUE(fp$is_numeric) && (fp$zero_fraction %||% 0) >= 0.3 &&
        !isTRUE(fp$is_near_constant)) {
      qual <- if (fp$zero_fraction >= 0.6) "strongly" else ""
      out[[length(out) + 1L]] <- .new_finding(
        section   = "inflation",
        type      = "zero_inflation",
        variables = nm,
        evidence  = list(zero_fraction = fp$zero_fraction),
        severity  = "notice",
        column    = nm,
        label     = sprintf(
          "%s is %szero-inflated", nm,
          if (nzchar(qual)) paste0(qual, " ") else ""
        )
      )
    }

    if (role == "measurement" && isTRUE(fp$is_numeric) &&
        !isTRUE(fp$is_near_constant) &&
        (fp$dominant_freq %||% 0) >= 0.5 &&
        (fp$zero_fraction %||% 0) < 0.3) {
      out[[length(out) + 1L]] <- .new_finding(
        section   = "inflation",
        type      = "dominant_values",
        variables = nm,
        evidence  = list(dominant_freq = fp$dominant_freq),
        severity  = "notice",
        column    = nm,
        label     = sprintf(
          "%s contains a small number of dominant extreme values", nm
        )
      )
    }

    if (role == "measurement" && isTRUE(fp$is_numeric) &&
        !isTRUE(fp$is_integerish) && fp$n_valid > 50L &&
        fp$n_unique <= 20L) {
      out[[length(out) + 1L]] <- .new_finding(
        section   = "inflation",
        type      = "discretized",
        variables = nm,
        evidence  = list(n_unique = fp$n_unique, n_valid = fp$n_valid),
        severity  = "notice",
        column    = nm,
        label     = sprintf(
          "%s appears discretized rather than continuously measured", nm
        )
      )
    }

    if (role %in% c("categorical", "group_id") && fp$n_valid > 30L) {
      vals <- df[[nm]][!is.na(df[[nm]])]
      tab  <- table(vals)
      n_singleton <- sum(tab == 1L)
      if (n_singleton >= 5L && n_singleton / length(tab) >= 0.3) {
        out[[length(out) + 1L]] <- .new_finding(
          section   = "inflation",
          type      = "singleton_levels",
          variables = nm,
          evidence  = list(
            n_singleton = n_singleton,
            n_levels    = length(tab)
          ),
          severity  = "notice",
          column    = nm,
          label     = sprintf("%s contains many singleton levels", nm)
        )
      }
    }
  }

  out
}
