# Relationship screening: numeric pairs, categorical~numeric pairs, and the
# rule layer that classifies each finding as meaningful, suspicious, or
# structural. Findings carry both numeric evidence and qualitative tags
# that drive the human-readable output.

.screen_relationships <- function(df, roles, fps, settings, primitives) {
  meas_cols <- names(roles)[roles %in% c("measurement", "compositional")]
  cat_cols  <- names(roles)[roles %in% c("categorical", "flag")]
  group_cols <- names(roles)[roles == "group_id"]
  temporal_cols <- names(roles)[roles == "temporal"]
  spatial_cols  <- names(roles)[roles %in% c("coord_lat", "coord_lon")]
  adj_cols  <- settings$adjustment %||% character(0)
  adj_cols  <- adj_cols[adj_cols %in% names(df)]

  ignored_cols <- names(roles)[roles %in% .ignored_roles]

  num_res    <- .numeric_pairs(df, meas_cols, adj_cols, settings, primitives)
  cat_res    <- .cat_num_pairs(df, c(cat_cols, group_cols), meas_cols,
                               settings, primitives, fps)
  drift_res  <- .drift_pairs(df, temporal_cols, spatial_cols,
                             settings, primitives)
  comp_res   <- .compositional_pairs(df, roles, settings, primitives)

  findings <- c(num_res$findings, cat_res$findings,
                drift_res$findings, comp_res$findings)

  findings <- .classify_findings(findings, roles, fps, settings)

  ignored_pairs <- c(num_res$ignored, cat_res$ignored,
                     drift_res$ignored, comp_res$ignored)

  list(
    findings         = findings,
    ignored_pairs    = ignored_pairs,
    ignored_cols     = ignored_cols,
    n_pairs_screened = length(findings) + length(ignored_pairs)
  )
}

# ---------------------------------------------------------------------------
# Numeric × numeric (the bulk of the work)
# ---------------------------------------------------------------------------

.numeric_pairs <- function(df, cols, adj_cols, settings, primitives) {
  findings <- list(); ignored <- list()
  if (length(cols) < 2L) return(list(findings = findings, ignored = ignored))

  # Drop adjustment columns from the screening list if present.
  cols <- setdiff(cols, adj_cols)
  if (length(cols) < 2L) return(list(findings = findings, ignored = ignored))

  pairs <- utils::combn(cols, 2L, simplify = FALSE)
  use_subsample <- nrow(df) > settings$subsample_threshold

  for (pair in pairs) {
    x_nm <- pair[[1]]; y_nm <- pair[[2]]
    res <- .screen_one_numeric_pair(df, x_nm, y_nm, adj_cols,
                                    settings, primitives, use_subsample)
    if (!is.null(res$ignored)) {
      ignored[[length(ignored) + 1L]] <- res$ignored
    } else if (!is.null(res$finding)) {
      findings[[length(findings) + 1L]] <- res$finding
    }
  }

  list(findings = findings, ignored = ignored)
}

.screen_one_numeric_pair <- function(df, x_nm, y_nm, adj_cols,
                                     settings, primitives, use_subsample) {
  x <- df[[x_nm]]; y <- df[[y_nm]]
  has_adj <- length(adj_cols) > 0L

  if (has_adj) {
    A  <- as.matrix(df[, adj_cols, drop = FALSE])
    ok <- which(stats::complete.cases(x, y, A))
  } else {
    ok <- which(stats::complete.cases(x, y))
  }

  if (length(ok) < settings$min_obs) {
    return(list(ignored = list(
      pair = c(x_nm, y_nm), reason = "insufficient_obs",
      n_complete = length(ok)
    )))
  }

  # Progressive subsampling: a small sample first; only if a signal appears
  # do we promote to a larger sample.
  if (use_subsample) {
    fit <- .progressive_screen(x[ok], y[ok], adj_cols, df, ok,
                               settings, primitives)
  } else {
    fit <- .fit_pair(x[ok], y[ok], adj_cols, df, ok, primitives)
  }
  if (is.null(fit) || is.null(fit$evidence)) return(list())

  ev <- fit$evidence
  list(finding = list(
    type      = "numeric_numeric",
    x         = x_nm,
    y         = y_nm,
    strength  = .classify_strength(abs(ev$r), settings),
    direction = if (ev$r >= 0) "positive" else "negative",
    shape     = "linear",
    evidence  = ev,
    adjusted  = has_adj,
    stability = fit$stability
  ))
}

# Two-stage subsampling: small probe, then larger confirmation if the probe
# shows enough signal. Stability is "high" if the two passes agree on
# strength tier, "medium" otherwise.
.progressive_screen <- function(x, y, adj_cols, df, ok,
                                settings, primitives) {
  n <- length(x)
  s1 <- min(settings$subsample_probe, n)
  s2 <- min(settings$subsample_confirm, n)

  set.seed(settings$seed)
  idx1 <- sample.int(n, s1)
  fit1 <- .fit_pair(x[idx1], y[idx1], adj_cols, df, ok[idx1], primitives)
  if (is.null(fit1$evidence)) return(NULL)

  if (abs(fit1$evidence$r) < settings$weak_threshold) {
    # Weak even on a probe, drop early. We keep the result so the screen
    # is still recorded as having happened.
    return(list(evidence = fit1$evidence, stability = "low"))
  }

  if (s2 > s1) {
    idx2 <- sample.int(n, s2)
    fit2 <- .fit_pair(x[idx2], y[idx2], adj_cols, df, ok[idx2], primitives)
    if (is.null(fit2$evidence)) {
      return(list(evidence = fit1$evidence, stability = "low"))
    }
    same_tier <- .classify_strength(abs(fit1$evidence$r), settings) ==
                 .classify_strength(abs(fit2$evidence$r), settings)
    return(list(
      evidence  = fit2$evidence,
      stability = if (same_tier) "high" else "medium"
    ))
  }

  list(evidence = fit1$evidence, stability = "high")
}

.fit_pair <- function(x_ok, y_ok, adj_cols, df, ok, primitives) {
  if (length(adj_cols) > 0L) {
    A_ok <- as.matrix(df[ok, adj_cols, drop = FALSE])
    x_sc <- as.numeric(primitives$residualize(x_ok, A_ok))
    y_sc <- as.numeric(primitives$residualize(y_ok, A_ok))
  } else {
    x_sc <- x_ok; y_sc <- y_ok
  }
  ev <- primitives$lm_simple(y_sc, x_sc)
  list(evidence = ev, stability = "high")
}

# ---------------------------------------------------------------------------
# Categorical / group_id × numeric
# ---------------------------------------------------------------------------

.cat_num_pairs <- function(df, cat_cols, num_cols, settings, primitives, fps) {
  findings <- list(); ignored <- list()

  for (cat_nm in cat_cols) {
    for (num_nm in num_cols) {
      g  <- df[[cat_nm]]; y <- df[[num_nm]]
      ok <- which(!is.na(g) & !is.na(y))

      if (length(ok) < settings$min_obs) {
        ignored[[length(ignored) + 1L]] <- list(
          pair = c(cat_nm, num_nm), reason = "insufficient_obs",
          n_complete = length(ok)
        )
        next
      }

      fit <- primitives$group_summary(y[ok], as.character(g[ok]))
      if (is.null(fit) || is.null(fit$eta2) || !is.numeric(fit$eta2)) next

      findings[[length(findings) + 1L]] <- list(
        type      = "categorical_numeric",
        x         = cat_nm,
        y         = num_nm,
        strength  = .classify_strength(sqrt(fit$eta2), settings),
        direction = NA_character_,
        shape     = "group_difference",
        evidence  = fit,
        adjusted  = FALSE,
        stability = "high",
        n_levels  = fit$k
      )
    }
  }

  list(findings = findings, ignored = ignored)
}

# ---------------------------------------------------------------------------
# Drift: temporal × spatial co-trend.
# A meaningful systematic drift implies the spatial sample shifts with time.
# ---------------------------------------------------------------------------

.drift_pairs <- function(df, temporal_cols, spatial_cols, settings, primitives) {
  findings <- list(); ignored <- list()
  if (length(temporal_cols) == 0L || length(spatial_cols) == 0L) {
    return(list(findings = findings, ignored = ignored))
  }

  for (t_nm in temporal_cols) {
    for (s_nm in spatial_cols) {
      t_raw <- df[[t_nm]]
      t_num <- if (inherits(t_raw, c("Date", "POSIXct", "POSIXlt"))) {
        as.numeric(t_raw)
      } else {
        as.numeric(t_raw)
      }
      s <- df[[s_nm]]
      ok <- which(stats::complete.cases(t_num, s))
      if (length(ok) < settings$min_obs) next

      fit <- primitives$lm_simple(s[ok], t_num[ok])
      if (is.null(fit)) next

      findings[[length(findings) + 1L]] <- list(
        type      = "drift",
        x         = t_nm,
        y         = s_nm,
        strength  = .classify_strength(abs(fit$r), settings),
        direction = if (fit$r >= 0) "positive" else "negative",
        shape     = "temporal_spatial_drift",
        evidence  = fit,
        adjusted  = FALSE,
        stability = "high"
      )
    }
  }
  list(findings = findings, ignored = ignored)
}

# ---------------------------------------------------------------------------
# Compositional: pairs of compositional columns whose sum is approximately
# constant. Treated as a structural relationship rather than a finding.
# ---------------------------------------------------------------------------

.compositional_pairs <- function(df, roles, settings, primitives) {
  findings <- list(); ignored <- list()
  comp_cols <- names(roles)[roles == "compositional"]
  if (length(comp_cols) < 2L) return(list(findings = findings, ignored = ignored))

  pairs <- utils::combn(comp_cols, 2L, simplify = FALSE)
  for (pair in pairs) {
    x <- df[[pair[[1]]]]; y <- df[[pair[[2]]]]
    ok <- which(stats::complete.cases(x, y))
    if (length(ok) < settings$min_obs) next

    s <- x[ok] + y[ok]
    if (length(unique(round(s, 4))) == 1L && abs(stats::sd(s)) < 1e-6) {
      # Exact constant sum
      cv <- 0
    } else {
      mu <- mean(s); sg <- stats::sd(s)
      cv <- if (mu > 0) sg / mu else Inf
    }

    if (cv < settings$compositional_cv) {
      findings[[length(findings) + 1L]] <- list(
        type      = "compositional",
        x         = pair[[1]],
        y         = pair[[2]],
        strength  = "structural",
        direction = "negative",
        shape     = "constrained_complement",
        evidence  = list(cv = cv, n = length(ok), sum_mean = mean(s)),
        adjusted  = FALSE,
        stability = "high"
      )
    }
  }
  list(findings = findings, ignored = ignored)
}

# ---------------------------------------------------------------------------
# Classification layer: turns findings into meaningful / suspicious /
# structural / negligible.
# ---------------------------------------------------------------------------

.classify_findings <- function(findings, roles, fps, settings) {
  lapply(findings, function(f) {
    f$kind <- .finding_kind(f, roles, fps, settings)
    f$concern <- .finding_concern(f, roles, fps, settings)
    f
  })
}

.finding_kind <- function(f, roles, fps, settings) {
  if (f$type == "compositional")          return("structural")
  if (f$type == "drift") {
    if (f$strength %in% c("strong", "moderate")) return("suspicious")
    return("negligible")
  }
  if (f$type == "categorical_numeric") {
    n_lvl <- f$n_levels %||% 0L
    # An id-like grouping column with many levels and a strong group effect
    # is an observer-effect-style suspicious pattern.
    is_idlike <- isTRUE(roles[[f$x]] == "group_id") || .name_is_id(f$x)
    if (is_idlike && n_lvl >= settings$observer_min_levels &&
        f$strength %in% c("strong", "moderate")) {
      return("suspicious")
    }
    if (f$strength %in% c("strong", "moderate")) return("meaningful")
    return("negligible")
  }
  # numeric_numeric default
  if (f$strength %in% c("strong", "moderate")) return("meaningful")
  if (f$strength == "weak")                    return("weak")
  "negligible"
}

.finding_concern <- function(f, roles, fps, settings) {
  if (f$type == "compositional")
    return("compositional relationship")
  if (f$type == "drift")
    return("sampling design may be spatially structured")
  if (f$type == "categorical_numeric") {
    if (f$kind %||% "" == "suspicious") return("possible observer effect")
    return(NA_character_)
  }
  NA_character_
}

.classify_strength <- function(effect, settings) {
  if      (effect >= settings$strong_threshold)   "strong"
  else if (effect >= settings$moderate_threshold) "moderate"
  else if (effect >= settings$weak_threshold)     "weak"
  else                                            "negligible"
}

# Null-coalescing helper (base R has no %||% before 4.4)
`%||%` <- function(a, b) if (!is.null(a)) a else b
