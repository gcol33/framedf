# Internal anomaly detection on individual columns.

.detect_anomalies <- function(df, roles, settings) {
  meas_cols <- names(roles[roles == "measurement"])
  findings  <- list()
  for (nm in meas_cols) {
    findings <- c(findings, .check_column(df[[nm]], nm, settings))
  }
  findings
}

.check_column <- function(col, name, settings) {
  findings  <- list()
  col_valid <- col[!is.na(col)]
  n         <- length(col_valid)
  if (n < 4L) return(findings)

  # Tukey fence outliers
  q   <- quantile(col_valid, c(0.25, 0.75))
  iqr <- q[[2]] - q[[1]]
  if (iqr > 0) {
    lo  <- q[[1]] - 1.5 * iqr
    hi  <- q[[2]] + 1.5 * iqr
    idx <- which(!is.na(col) & (col < lo | col > hi))
    if (length(idx) > 0L) {
      findings[[length(findings) + 1L]] <- list(
        column  = name,
        type    = "outlier",
        method  = "tukey_iqr",
        n       = length(idx),
        indices = idx,
        bounds  = c(lo, hi)
      )
    }
  }

  # Moment-based skewness
  mu   <- mean(col_valid)
  m2   <- mean((col_valid - mu)^2)
  m3   <- mean((col_valid - mu)^3)
  skew <- if (m2 > 0) m3 / m2^(3/2) else 0

  if (abs(skew) > settings$skew_threshold) {
    findings[[length(findings) + 1L]] <- list(
      column    = name,
      type      = "skewness",
      method    = "moment",
      skewness  = skew,
      direction = if (skew > 0) "right" else "left"
    )
  }

  findings
}
