# Missingness detection.
#
# We surface three patterns:
#   - systematic missingness over time (a missing-indicator that drifts
#     monotonically with a temporal column)
#   - clustered missingness by group (the missing-share differs across
#     levels of a categorical / grouping column)
#   - jointly missing variables (two or more columns whose missingness
#     patterns coincide at high rate)
#
# All three are cheap correlations on the boolean missing-indicator.

.detect_missingness <- function(df, roles, fps, settings) {
  candidates <- names(fps)[vapply(fps, function(fp) {
    (fp$prop_missing %||% 0) >= 0.05 &&
      (fp$prop_missing %||% 0) <= 0.95
  }, logical(1L))]
  if (length(candidates) == 0L) return(list())

  temporal <- names(roles)[roles == "temporal"]
  groupers <- names(roles)[roles %in% c("group_id", "categorical")]
  groupers <- groupers[vapply(groupers, function(nm) {
    nu <- fps[[nm]]$n_unique
    !is.null(nu) && nu >= 2L && nu <= 200L
  }, logical(1L))]

  out <- list()

  for (nm in candidates) {
    miss_ind <- as.integer(is.na(df[[nm]]))

    for (t_nm in temporal) {
      t_raw <- df[[t_nm]]
      t_num <- if (inherits(t_raw, c("Date", "POSIXct", "POSIXlt"))) {
        as.numeric(t_raw)
      } else {
        suppressWarnings(as.numeric(t_raw))
      }
      ok <- which(!is.na(t_num))
      if (length(ok) < 30L) next
      cor_t <- suppressWarnings(stats::cor(miss_ind[ok], t_num[ok]))
      if (is.na(cor_t)) next
      if (abs(cor_t) >= 0.2) {
        direction <- if (cor_t > 0) "newer" else "older"
        out[[length(out) + 1L]] <- .new_finding(
          section   = "missingness",
          type      = "missingness_temporal",
          variables = c(nm, t_nm),
          evidence  = list(cor = cor_t, n = length(ok)),
          severity  = "notice",
          column    = nm,
          temporal  = t_nm,
          direction = direction,
          label     = sprintf(
            "%s is missing systematically in %s observations",
            nm, direction
          )
        )
      }
    }

    for (g_nm in groupers) {
      g <- df[[g_nm]]
      ok <- which(!is.na(g))
      if (length(ok) < 30L) next
      tab <- tapply(miss_ind[ok], g[ok], mean)
      if (length(tab) < 2L) next
      if (max(tab) - min(tab) >= 0.2 && stats::sd(tab) > 0.1) {
        out[[length(out) + 1L]] <- .new_finding(
          section   = "missingness",
          type      = "missingness_clustered",
          variables = c(nm, g_nm),
          evidence  = list(spread = max(tab) - min(tab),
                           sd_levels = stats::sd(tab)),
          severity  = "notice",
          column    = nm,
          grouped_by = g_nm,
          label     = sprintf(
            "%s shows clustered missingness by %s", nm, g_nm
          )
        )
      }
    }
  }

  joint <- .detect_joint_missingness(df, candidates)
  out <- c(out, joint)

  out
}

.detect_joint_missingness <- function(df, candidates) {
  if (length(candidates) < 2L) return(list())
  M <- vapply(candidates, function(nm) is.na(df[[nm]]),
              logical(nrow(df)))
  if (!is.matrix(M)) M <- matrix(M, ncol = length(candidates),
                                 dimnames = list(NULL, candidates))
  out <- list()
  pairs <- utils::combn(candidates, 2L, simplify = FALSE)
  for (pair in pairs) {
    a <- M[, pair[[1L]]]; b <- M[, pair[[2L]]]
    if (sum(a) < 10L || sum(b) < 10L) next
    co <- suppressWarnings(stats::cor(a, b))
    if (is.na(co)) next
    if (co >= 0.7) {
      out[[length(out) + 1L]] <- .new_finding(
        section   = "missingness",
        type      = "missingness_joint",
        variables = pair,
        evidence  = list(cor = co),
        severity  = "notice",
        label     = sprintf(
          "%s and %s appear jointly missing", pair[[1L]], pair[[2L]]
        )
      )
    }
  }
  out
}
