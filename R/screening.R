# Relationship screening: numeric-numeric and categorical-numeric pairs.
# All functions return findings lists with evidence stored internally.

.screen_relationships <- function(df, roles, settings, primitives) {
  meas_cols <- names(roles[roles == "measurement"])
  cat_cols  <- names(roles[roles %in% c("categorical", "flag")])
  adj_cols  <- settings$adjustment %||% character(0)
  adj_cols  <- adj_cols[adj_cols %in% names(df)]

  # Columns ignored from screening (logged for details/print)
  ignored_cols <- names(roles[roles %in% .ignored_roles])

  num_res    <- .numeric_pairs(df, meas_cols, adj_cols, settings, primitives)
  cat_res    <- .cat_num_pairs(df, cat_cols, meas_cols, settings, primitives)

  list(
    findings             = c(num_res$findings, cat_res$findings),
    ignored_pairs        = c(num_res$ignored,  cat_res$ignored),
    ignored_cols         = ignored_cols,
    roles                = roles,
    n_pairs_screened     = length(num_res$findings) + length(cat_res$findings) +
                           length(num_res$ignored)  + length(cat_res$ignored)
  )
}

.numeric_pairs <- function(df, cols, adj_cols, settings, primitives) {
  findings <- list(); ignored <- list()
  if (length(cols) < 2L) return(list(findings = findings, ignored = ignored))

  pairs <- combn(cols, 2L, simplify = FALSE)

  for (pair in pairs) {
    x_nm <- pair[[1]]; y_nm <- pair[[2]]
    x <- df[[x_nm]];   y <- df[[y_nm]]

    if (length(adj_cols) > 0L) {
      A  <- as.matrix(df[, adj_cols, drop = FALSE])
      ok <- which(complete.cases(x, y, A))
    } else {
      ok <- which(complete.cases(x, y))
    }

    if (length(ok) < settings$min_obs) {
      ignored[[length(ignored) + 1L]] <- list(
        pair = pair, reason = "insufficient_obs", n_complete = length(ok)
      )
      next
    }

    if (length(adj_cols) > 0L) {
      A_ok <- as.matrix(df[ok, adj_cols, drop = FALSE])
      x_sc <- as.numeric(primitives$residualize(x[ok], A_ok))
      y_sc <- as.numeric(primitives$residualize(y[ok], A_ok))
      adjusted <- TRUE
    } else {
      x_sc <- x[ok]; y_sc <- y[ok]
      adjusted <- FALSE
    }

    fit <- primitives$lm_simple(y_sc, x_sc)
    if (is.null(fit)) next

    findings[[length(findings) + 1L]] <- list(
      type      = "numeric_numeric",
      x         = x_nm,
      y         = y_nm,
      strength  = .classify_strength(abs(fit$r), settings),
      direction = if (fit$beta1 >= 0) "positive" else "negative",
      shape     = "linear",
      evidence  = fit,
      adjusted  = adjusted
    )
  }

  list(findings = findings, ignored = ignored)
}

.cat_num_pairs <- function(df, cat_cols, num_cols, settings, primitives) {
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

      fit <- primitives$group_summary(y[ok], g[ok])
      if (is.null(fit)) next

      findings[[length(findings) + 1L]] <- list(
        type      = "categorical_numeric",
        x         = cat_nm,
        y         = num_nm,
        strength  = .classify_strength(sqrt(fit$eta2), settings),
        direction = NA_character_,
        shape     = "group_difference",
        evidence  = fit,
        adjusted  = FALSE
      )
    }
  }

  list(findings = findings, ignored = ignored)
}

.classify_strength <- function(effect, settings) {
  if      (effect >= settings$strong_threshold)   "strong"
  else if (effect >= settings$moderate_threshold) "moderate"
  else if (effect >= settings$weak_threshold)     "weak"
  else                                            "negligible"
}

# Null-coalescing helper (base R has no %||% before 4.4)
`%||%` <- function(a, b) if (!is.null(a)) a else b
