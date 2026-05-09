# Decide whether a compositional column is on a [0, 1] or [0, 100] scale.
# Stragglers above 1 are tolerated when most of the distribution sits below.
.composition_scale <- function(vals) {
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) return(1)
  q <- stats::quantile(vals, 0.95, names = FALSE)
  if (q <= 1.05) 1 else 100
}

# Per-column anomaly detection.
# We separate anomaly flavours by column role:
#   measurement / compositional → outliers, skew, range plausibility
#   coord_lat / coord_lon       → range plausibility
#   compositional pairs         → totals exceeding bounds
#   categorical                 → inconsistent capitalization, very rare levels
# Each finding returns a small list with a `tag` describing the qualitative
# pattern (used by print) and any structured detail needed by anomalies().

.detect_anomalies <- function(df, roles, fps, settings) {
  findings <- list()

  for (nm in names(df)) {
    role <- roles[[nm]]
    col  <- df[[nm]]

    if (role %in% c("measurement", "compositional")) {
      findings <- c(findings, .check_numeric(col, nm, settings))
    }
    if (role %in% c("coord_lat", "coord_lon")) {
      findings <- c(findings, .check_coord(col, nm, role))
    }
    if (role == "compositional") {
      findings <- c(findings, .check_composition_bounds(col, nm))
    }
    if (role == "categorical") {
      findings <- c(findings, .check_categorical(col, nm, settings))
    }
    if (role == "temporal") {
      findings <- c(findings, .check_temporal_gap(col, nm))
    }
  }

  findings <- c(findings, .check_composition_pairs(df, roles))
  findings <- c(findings, .check_coord_swap(df, roles))

  findings
}

.check_numeric <- function(col, name, settings) {
  out <- list()
  vals <- col[!is.na(col)]
  n    <- length(vals)
  if (n < 4L) return(out)

  q   <- stats::quantile(vals, c(0.25, 0.75))
  iqr <- q[[2]] - q[[1]]
  if (iqr > 0) {
    lo  <- q[[1]] - 1.5 * iqr
    hi  <- q[[2]] + 1.5 * iqr
    idx <- which(!is.na(col) & (col < lo | col > hi))
    if (length(idx) > 0L) {
      out[[length(out) + 1L]] <- list(
        column = name, type = "outlier", method = "tukey_iqr",
        n = length(idx), indices = idx, bounds = c(lo, hi),
        tag = "outliers"
      )
    }
  }

  mu   <- mean(vals)
  m2   <- mean((vals - mu)^2)
  m3   <- mean((vals - mu)^3)
  skew <- if (m2 > 0) m3 / m2^(3 / 2) else 0
  if (abs(skew) > settings$skew_threshold) {
    out[[length(out) + 1L]] <- list(
      column = name, type = "skewness", method = "moment",
      skewness = skew, direction = if (skew > 0) "right" else "left",
      tag = "skewed_distribution"
    )
  }

  out
}

.check_coord <- function(col, name, role) {
  vals <- col[!is.na(col)]
  if (length(vals) == 0L) return(list())
  rng <- range(vals)
  bound <- if (role == "coord_lat") c(-90, 90) else c(-180, 180)
  if (rng[1] < bound[1] || rng[2] > bound[2]) {
    return(list(list(
      column = name, type = "implausible_range",
      method = "coord_bounds", bounds = bound,
      observed = rng, tag = "implausible_values"
    )))
  }
  list()
}

.check_composition_bounds <- function(col, name) {
  vals <- col[!is.na(col)]
  if (length(vals) == 0L) return(list())
  scale <- .composition_scale(vals)
  too_high <- which(!is.na(col) & col > scale + 1e-6)
  too_low  <- which(!is.na(col) & col < -1e-6)
  out <- list()
  if (length(too_high) > 0L) {
    out[[length(out) + 1L]] <- list(
      column = name, type = "out_of_bounds",
      direction = "above", n = length(too_high),
      indices = too_high, scale = scale,
      tag = "exceed_expected_totals"
    )
  }
  if (length(too_low) > 0L) {
    out[[length(out) + 1L]] <- list(
      column = name, type = "out_of_bounds",
      direction = "below", n = length(too_low),
      indices = too_low, scale = scale,
      tag = "exceed_expected_totals"
    )
  }
  out
}

.check_composition_pairs <- function(df, roles) {
  comp_cols <- names(roles)[roles == "compositional"]
  if (length(comp_cols) < 2L) return(list())
  out <- list()
  pairs <- utils::combn(comp_cols, 2L, simplify = FALSE)
  for (pair in pairs) {
    x <- df[[pair[[1]]]]; y <- df[[pair[[2]]]]
    ok <- which(stats::complete.cases(x, y))
    if (length(ok) == 0L) next
    scale <- .composition_scale(c(x[ok], y[ok]))
    over <- which(x[ok] + y[ok] > scale + 1e-6)
    if (length(over) > 0L) {
      out[[length(out) + 1L]] <- list(
        column = paste(pair, collapse = "+"),
        type   = "pair_exceeds_total",
        n      = length(over),
        indices = ok[over],
        scale  = scale,
        cols   = pair,
        tag    = "exceed_expected_totals"
      )
    }
  }
  out
}

.check_temporal_gap <- function(col, name) {
  vals <- if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    as.numeric(col[!is.na(col)])
  } else {
    suppressWarnings(as.numeric(col[!is.na(col)]))
  }
  vals <- vals[!is.na(vals)]
  if (length(vals) < 30L) return(list())

  q   <- stats::quantile(vals, c(0.05, 0.95))
  iqr <- q[[2L]] - q[[1L]]
  if (iqr <= 0) return(list())
  lo  <- q[[1L]] - 1.5 * iqr
  hi  <- q[[2L]] + 1.5 * iqr
  outside <- which(vals < lo | vals > hi)
  if (length(outside) == 0L || length(outside) > length(vals) * 0.05) {
    return(list())
  }
  list(list(
    column = name, type = "temporal_gap",
    method = "core_period", n = length(outside),
    bounds = c(lo, hi),
    tag    = "isolated_temporal"
  ))
}

# Detect coordinates that may have been swapped: a column tagged coord_lat
# whose values look more like longitude (and vice versa). We look at the
# range and require both columns to exist.
.check_coord_swap <- function(df, roles) {
  lat_cols <- names(roles)[roles == "coord_lat"]
  lon_cols <- names(roles)[roles == "coord_lon"]
  if (length(lat_cols) == 0L || length(lon_cols) == 0L) return(list())

  out <- list()
  for (lat_nm in lat_cols) {
    for (lon_nm in lon_cols) {
      lat <- df[[lat_nm]]; lon <- df[[lon_nm]]
      ok  <- !is.na(lat) & !is.na(lon)
      if (sum(ok) < 30L) next
      lat_v <- lat[ok]; lon_v <- lon[ok]
      lat_outside <- mean(abs(lat_v) > 90)
      lon_resembles_lat <- mean(abs(lon_v) <= 90)
      lat_resembles_lon <- mean(abs(lat_v) > 90 & abs(lat_v) <= 180)
      if (lat_outside > 0.01 && lat_resembles_lon > 0.5 &&
          lon_resembles_lat > 0.5) {
        out[[length(out) + 1L]] <- list(
          column = paste(lat_nm, lon_nm, sep = "+"),
          type   = "possible_swap",
          method = "coord_range_check",
          cols   = c(lat_nm, lon_nm),
          n      = round(lat_outside * sum(ok)),
          tag    = "possible_coord_swap"
        )
      }
    }
  }
  out
}

.check_categorical <- function(col, name, settings) {
  out  <- list()
  vals <- if (is.factor(col)) levels(col) else unique(col[!is.na(col)])
  if (length(vals) == 0L) return(out)

  # Capitalization inconsistency: two levels that match after tolower.
  fold <- tolower(vals)
  dupes <- fold[duplicated(fold) | duplicated(fold, fromLast = TRUE)]
  if (length(unique(dupes)) > 0L) {
    out[[length(out) + 1L]] <- list(
      column = name, type = "case_inconsistency",
      examples = unique(dupes)[seq_len(min(5L, length(unique(dupes))))],
      tag = "inconsistent_capitalization"
    )
  }

  # Rare levels: levels with fewer than min_level_n rows.
  tab <- table(col, useNA = "no")
  rare <- names(tab)[tab > 0 & tab < settings$min_level_n]
  if (length(rare) > 0L) {
    out[[length(out) + 1L]] <- list(
      column = name, type = "rare_levels",
      n_rare = length(rare), examples = utils::head(rare, 5L),
      threshold = settings$min_level_n,
      tag = "very_few_rows_levels"
    )
  }

  out
}
