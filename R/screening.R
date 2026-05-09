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
  conf_res   <- .confounder_pairs(df, meas_cols, c(cat_cols, group_cols),
                                  settings, primitives)
  nonlin_res <- .nonlinear_pairs(df, meas_cols, adj_cols,
                                 settings, primitives)
  catcat_res <- .cat_cat_pairs(df, c(cat_cols, group_cols),
                               settings, primitives, fps)

  findings <- c(num_res$findings, cat_res$findings,
                drift_res$findings, comp_res$findings,
                conf_res$findings, nonlin_res$findings,
                catcat_res$findings)

  findings <- .classify_findings(findings, roles, fps, settings)
  findings <- .merge_multi_target(findings, roles)

  ignored_pairs <- c(num_res$ignored, cat_res$ignored,
                     drift_res$ignored, comp_res$ignored,
                     conf_res$ignored, nonlin_res$ignored,
                     catcat_res$ignored)

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
  list(finding = .new_finding(
    section    = "relationships",
    type       = "numeric_numeric",
    variables  = c(x_nm, y_nm),
    evidence   = ev,
    confidence = .stability_to_confidence(fit$stability),
    severity   = "info",
    x          = x_nm,
    y          = y_nm,
    strength   = .classify_strength(abs(ev$r), settings),
    direction  = if (ev$r >= 0) "positive" else "negative",
    shape      = "linear",
    adjusted   = has_adj,
    stability  = fit$stability,
    raw_r      = fit$raw_r %||% NA_real_,
    adjusted_r = fit$adjusted_r %||% NA_real_,
    adjustment_vars = adj_cols
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
  raw_r <- adjusted_r <- NA_real_
  if (length(adj_cols) > 0L) {
    raw_ev <- primitives$lm_simple(y_ok, x_ok)
    if (!is.null(raw_ev)) raw_r <- raw_ev$r
    A_ok <- as.matrix(df[ok, adj_cols, drop = FALSE])
    x_sc <- as.numeric(primitives$residualize(x_ok, A_ok))
    y_sc <- as.numeric(primitives$residualize(y_ok, A_ok))
    ev <- primitives$lm_simple(y_sc, x_sc)
    if (!is.null(ev)) adjusted_r <- ev$r
  } else {
    ev <- primitives$lm_simple(y_ok, x_ok)
  }
  list(
    evidence   = ev,
    stability  = "high",
    raw_r      = raw_r,
    adjusted_r = adjusted_r
  )
}

.stability_to_confidence <- function(stability) {
  switch(stability %||% "",
    high   = "high",
    medium = "moderate",
    low    = "low",
    "moderate"
  )
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

      findings[[length(findings) + 1L]] <- .new_finding(
        section   = "relationships",
        type      = "categorical_numeric",
        variables = c(cat_nm, num_nm),
        evidence  = fit,
        x         = cat_nm,
        y         = num_nm,
        strength  = .classify_strength(sqrt(fit$eta2), settings),
        direction = NA_character_,
        shape     = "group_difference",
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

      findings[[length(findings) + 1L]] <- .new_finding(
        section   = "relationships",
        type      = "drift",
        variables = c(t_nm, s_nm),
        evidence  = fit,
        x         = t_nm,
        y         = s_nm,
        strength  = .classify_strength(abs(fit$r), settings),
        direction = if (fit$r >= 0) "positive" else "negative",
        shape     = "temporal_spatial_drift",
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
      findings[[length(findings) + 1L]] <- .new_finding(
        section   = "relationships",
        type      = "compositional",
        variables = c(pair[[1]], pair[[2]]),
        evidence  = list(cv = cv, n = length(ok), sum_mean = mean(s)),
        confidence = "high",
        x         = pair[[1]],
        y         = pair[[2]],
        strength  = "structural",
        direction = "negative",
        shape     = "constrained_complement",
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
    f$kind    <- .finding_kind(f, roles, fps, settings)
    f$concern <- .finding_concern(f, roles, fps, settings)
    if (!is.null(f$kind) && f$kind == "suspicious") f$severity <- "notice"
    .label_finding(f)
  })
}

.finding_kind <- function(f, roles, fps, settings) {
  if (f$type == "compositional")          return("structural")
  if (f$type == "drift") {
    if (f$strength %in% c("strong", "moderate")) return("suspicious")
    return("negligible")
  }
  if (f$type == "confounded")             return("suspicious")
  if (f$type == "nonlinear")              return("suspicious")
  if (f$type == "multi_target")           return("suspicious")
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
  if (f$type == "cat_cat") {
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
  if (f$type == "drift") {
    if (f$strength %in% c("strong", "moderate")) {
      return("possible spatial sampling drift")
    }
    return(NA_character_)
  }
  if (f$type == "confounded")
    return("possible regional confounding")
  if (f$type == "nonlinear")
    return("possible saturation effect")
  if (f$type == "multi_target")
    return("possible dataset effect")
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

# ---------------------------------------------------------------------------
# Confounded numeric pairs.
# For each numeric pair, fit y ~ x and compare against y ~ x | Z, where Z
# is each candidate confounder (low-cardinality categorical / group_id /
# spatial / temporal). When the adjusted strength is meaningfully smaller
# than the raw strength, emit a `confounded` finding.
# ---------------------------------------------------------------------------

.confounder_pairs <- function(df, num_cols, cat_cols, settings, primitives) {
  findings <- list(); ignored <- list()
  if (length(num_cols) < 2L || length(cat_cols) == 0L) {
    return(list(findings = findings, ignored = ignored))
  }

  pairs <- utils::combn(num_cols, 2L, simplify = FALSE)
  n     <- nrow(df)

  for (pair in pairs) {
    x_nm <- pair[[1L]]; y_nm <- pair[[2L]]
    x <- df[[x_nm]]; y <- df[[y_nm]]
    ok <- which(stats::complete.cases(x, y))
    if (length(ok) < settings$min_obs) next
    if (n > settings$subsample_threshold) {
      set.seed(settings$seed)
      ok <- sample(ok, min(length(ok), settings$subsample_confirm))
    }
    raw_fit <- primitives$lm_simple(y[ok], x[ok])
    if (is.null(raw_fit) || !is.numeric(raw_fit$r)) next
    raw_strength <- abs(raw_fit$r)
    if (raw_strength < settings$moderate_threshold) next

    for (z_nm in cat_cols) {
      z <- df[[z_nm]]
      ok_z <- ok[!is.na(z[ok])]
      if (length(ok_z) < settings$min_obs) next
      lvls <- unique(z[ok_z])
      if (length(lvls) < 2L || length(lvls) > 50L) next

      Z <- stats::model.matrix(~ factor(z[ok_z]))[, -1L, drop = FALSE]
      if (ncol(Z) == 0L) next

      x_res <- as.numeric(primitives$residualize(x[ok_z], Z))
      y_res <- as.numeric(primitives$residualize(y[ok_z], Z))
      adj_fit <- primitives$lm_simple(y_res, x_res)
      if (is.null(adj_fit)) next

      adj_strength <- abs(adj_fit$r)
      if (raw_strength - adj_strength >= 0.25 &&
          adj_strength < settings$moderate_threshold) {
        findings[[length(findings) + 1L]] <- .new_finding(
          section   = "relationships",
          type      = "confounded",
          variables = c(x_nm, y_nm, z_nm),
          evidence  = list(
            raw_strength = raw_strength,
            adjusted_strength = adj_strength,
            method = "partial_lm_screen",
            n_used = length(ok_z)
          ),
          confidence = "moderate",
          severity   = "notice",
          x          = x_nm,
          y          = y_nm,
          strength   = .classify_strength(raw_strength, settings),
          direction  = if (raw_fit$r >= 0) "positive" else "negative",
          shape      = "confounded",
          adjusted   = TRUE,
          stability  = "high",
          adjustment_vars = z_nm
        )
        break
      }
    }
  }

  list(findings = findings, ignored = ignored)
}

# ---------------------------------------------------------------------------
# Nonlinear screen.
# Fit y ~ x and y ~ x + x^2; if quadratic explains substantially more
# variance, surface as a nonlinear finding.
# ---------------------------------------------------------------------------

.nonlinear_pairs <- function(df, num_cols, adj_cols, settings, primitives) {
  findings <- list(); ignored <- list()
  if (length(num_cols) < 2L) return(list(findings = findings, ignored = ignored))
  cols  <- setdiff(num_cols, adj_cols)
  if (length(cols) < 2L) return(list(findings = findings, ignored = ignored))

  pairs <- utils::combn(cols, 2L, simplify = FALSE)
  n     <- nrow(df)

  for (pair in pairs) {
    x_nm <- pair[[1L]]; y_nm <- pair[[2L]]
    x <- df[[x_nm]]; y <- df[[y_nm]]
    ok <- which(stats::complete.cases(x, y))
    if (length(ok) < 50L) next
    if (n > settings$subsample_threshold) {
      set.seed(settings$seed + 1L)
      ok <- sample(ok, min(length(ok), settings$subsample_confirm))
    }

    xs <- as.numeric(x[ok]); ys <- as.numeric(y[ok])
    lin_fit <- primitives$lm_simple(ys, xs)
    if (is.null(lin_fit)) next

    X2 <- cbind(xs, xs^2)
    quad_fit <- .lm_multiple_r(ys, X2)
    if (is.null(quad_fit)) next

    delta <- quad_fit$r2 - (lin_fit$r2 %||% 0)
    if (delta >= 0.10 && quad_fit$r2 >= 0.30) {
      findings[[length(findings) + 1L]] <- .new_finding(
        section   = "relationships",
        type      = "nonlinear",
        variables = c(x_nm, y_nm),
        evidence  = list(
          linear_r2    = lin_fit$r2,
          quadratic_r2 = quad_fit$r2,
          delta_r2     = delta,
          n_used       = length(ok)
        ),
        confidence = "moderate",
        severity   = "notice",
        x          = x_nm,
        y          = y_nm,
        strength   = .classify_strength(sqrt(quad_fit$r2), settings),
        direction  = NA_character_,
        shape      = "nonlinear",
        adjusted   = FALSE,
        stability  = "high"
      )
    }
  }

  list(findings = findings, ignored = ignored)
}

# ---------------------------------------------------------------------------
# Categorical × categorical screen.
# Cramer's V between two low-cardinality categorical / group_id columns.
# ---------------------------------------------------------------------------

.cat_cat_pairs <- function(df, cat_cols, settings, primitives, fps) {
  findings <- list(); ignored <- list()
  if (length(cat_cols) < 2L) return(list(findings = findings, ignored = ignored))

  usable <- cat_cols[vapply(cat_cols, function(nm) {
    nu <- fps[[nm]]$n_unique
    !is.null(nu) && nu >= 2L && nu <= 50L
  }, logical(1L))]
  if (length(usable) < 2L) return(list(findings = findings, ignored = ignored))

  pairs <- utils::combn(usable, 2L, simplify = FALSE)
  for (pair in pairs) {
    a <- df[[pair[[1L]]]]; b <- df[[pair[[2L]]]]
    ok <- which(!is.na(a) & !is.na(b))
    if (length(ok) < settings$min_obs) next
    v <- .cramers_v(a[ok], b[ok])
    if (is.na(v)) next

    findings[[length(findings) + 1L]] <- .new_finding(
      section   = "relationships",
      type      = "cat_cat",
      variables = pair,
      evidence  = list(cramers_v = v, n = length(ok)),
      x         = pair[[1L]],
      y         = pair[[2L]],
      strength  = .classify_strength(v, settings),
      direction = NA_character_,
      shape     = "cramers_v",
      adjusted  = FALSE,
      stability = "high"
    )
  }
  list(findings = findings, ignored = ignored)
}

.cramers_v <- function(a, b) {
  tab <- suppressWarnings(table(a, b))
  if (any(dim(tab) < 2L)) return(NA_real_)
  n   <- sum(tab)
  if (n == 0L) return(NA_real_)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))$statistic
  k   <- min(nrow(tab), ncol(tab)) - 1L
  if (k <= 0L) return(NA_real_)
  unname(sqrt(as.numeric(chi) / (n * k)))
}

# ---------------------------------------------------------------------------
# Multi-target merger.
# When a categorical column X has strong group effects on multiple
# numeric targets, collapse those individual findings into a single
# `multi_target` finding ("X structures both Y1 and Y2"). The originals
# are dropped to avoid double counting.
# ---------------------------------------------------------------------------

.merge_multi_target <- function(findings, roles) {
  if (length(findings) == 0L) return(findings)

  is_cn <- vapply(findings, function(f) {
    isTRUE(f$type == "categorical_numeric") &&
      isTRUE(f$kind %in% c("meaningful", "suspicious"))
  }, logical(1L))
  if (sum(is_cn) < 2L) return(findings)

  cn <- findings[is_cn]
  by_x <- split(cn, vapply(cn, `[[`, character(1L), "x"))

  to_drop <- integer(0)
  to_add  <- list()
  for (x_nm in names(by_x)) {
    grp <- by_x[[x_nm]]
    if (length(grp) < 2L) next
    ys      <- vapply(grp, `[[`, character(1L), "y")
    is_id   <- isTRUE(roles[[x_nm]] == "group_id") || .name_is_id(x_nm)
    is_part <- isTRUE(.name_is_partition(x_nm)) ||
               isTRUE(roles[[x_nm]] == "categorical")
    if (!(is_id || is_part)) next

    concern_text <- if (grepl(
      "observer|annotator|surveyor|recorder|inspector", tolower(x_nm))) {
      "possible observer effect"
    } else if (grepl("country|region|state|province", tolower(x_nm))) {
      "possible regional effect"
    } else {
      "possible dataset effect"
    }

    add <- .new_finding(
      section   = "relationships",
      type      = "multi_target",
      variables = c(x_nm, ys),
      evidence  = list(targets = ys, n_targets = length(ys)),
      confidence = "moderate",
      severity   = "notice",
      x         = x_nm,
      y         = paste(ys, collapse = ","),
      strength  = grp[[1L]]$strength,
      direction = NA_character_,
      shape     = "multi_target",
      adjusted  = FALSE,
      stability = "high",
      kind      = "suspicious",
      concern   = concern_text
    )
    add <- .label_finding(add)
    to_add[[length(to_add) + 1L]] <- add

    cn_idx <- which(is_cn)
    drop_y <- ys
    for (i in cn_idx) {
      f <- findings[[i]]
      if (isTRUE(f$x == x_nm) && f$y %in% drop_y) {
        to_drop <- c(to_drop, i)
      }
    }
  }

  if (length(to_drop) > 0L) findings <- findings[-to_drop]
  c(findings, to_add)
}

