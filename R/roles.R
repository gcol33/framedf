# Role inference: assign a semantic role to each column.
# Roles drive which columns are screened, what language is used to describe
# them, and which structural patterns we expect (temporal, spatial,
# compositional, grouping). All role logic is deliberately conservative:
# when in doubt, fall back to "measurement" or "categorical".

.role_levels <- c(
  "id", "admin_index", "group_id",
  "temporal", "coord_lat", "coord_lon",
  "compositional", "measurement", "ratio_unit",
  "categorical", "flag",
  "near_constant", "constant",
  "date", "unknown"
)

.infer_roles <- function(df, settings) {
  nrow_df <- nrow(df)
  raw <- vapply(names(df), function(nm) {
    .classify_column(df[[nm]], nm, nrow_df, settings)
  }, character(1L))
  names(raw) <- names(df)

  # Compositional refinement: a numeric column in [0, 100] or [0, 1]
  # whose name looks like a share (cover, percent, prop, fraction, share)
  # is tagged compositional. We do this in a second pass so we can prefer
  # the strongest cue (range + name).
  comp <- vapply(names(df), function(nm) {
    if (raw[[nm]] != "measurement") return(FALSE)
    .looks_compositional(df[[nm]], nm)
  }, logical(1L))
  raw[comp] <- "compositional"

  raw
}

.classify_column <- function(col, name, nrow_df, settings) {
  if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) return("temporal")
  if (is.logical(col)) {
    n_uniq <- length(unique(col[!is.na(col)]))
    if (n_uniq <= 1L) return("constant")
    return("flag")
  }

  if (is.numeric(col)) {
    vals    <- col[!is.na(col)]
    n_valid <- length(vals)
    n_uniq  <- length(unique(vals))

    if (n_uniq <= 1L) return("constant")

    if (n_valid > 0) {
      tab     <- tabulate(match(vals, unique(vals)))
      dom_freq <- max(tab) / n_valid
      if (dom_freq > 1 - settings$near_constant_ratio) return("near_constant")
    }

    nm_lo <- tolower(name)

    # Coordinates: detected by both name and plausible range.
    if (.name_is_lat(nm_lo) && .range_is_lat(vals))   return("coord_lat")
    if (.name_is_lon(nm_lo) && .range_is_lon(vals))   return("coord_lon")

    # Year-like temporal column: integer in a plausible calendar range.
    if (.is_year_like(col, name, nrow_df))            return("temporal")

    # Sequential administrative index (row_number / index / order / ...)
    if (is.integer(col) && n_uniq == nrow_df && .name_is_admin_index(name)) {
      return("admin_index")
    }

    # General identifier: integer typed, all unique, name suggests an id.
    if (n_uniq == nrow_df && is.integer(col) && .name_is_id(name)) {
      return("id")
    }

    # Repeated integer with id-like name → grouping identifier.
    if (is.integer(col) && n_uniq < nrow_df && .name_is_id(name)) {
      return("group_id")
    }

    return("measurement")
  }

  if (is.character(col) || is.factor(col)) {
    vals    <- if (is.factor(col)) levels(col) else unique(col[!is.na(col)])
    n_valid <- sum(!is.na(col))
    n_uniq  <- length(vals)

    # Very high-cardinality character → identifier.
    if (n_valid > 0 && n_uniq / n_valid > settings$id_unique_ratio) return("id")

    # Mid-cardinality character with id-like name → grouping identifier.
    if (n_uniq > 1L && .name_is_id(name)) return("group_id")

    return("categorical")
  }

  "unknown"
}

# Per-column descriptive statistics stored for later display and rule logic.
.fingerprint_columns <- function(df) {
  lapply(names(df), function(nm) {
    col     <- df[[nm]]
    n_valid <- sum(!is.na(col))
    n_miss  <- sum(is.na(col))
    n_uniq  <- length(unique(col[!is.na(col)]))

    fp <- list(
      name     = nm,
      class    = class(col),
      n_valid  = n_valid,
      n_miss   = n_miss,
      n_unique = n_uniq
    )

    if (is.numeric(col) && n_valid > 0) {
      fp$mean   <- mean(col, na.rm = TRUE)
      fp$sd     <- stats::sd(col, na.rm = TRUE)
      fp$min    <- min(col, na.rm = TRUE)
      fp$max    <- max(col, na.rm = TRUE)
      fp$median <- stats::median(col, na.rm = TRUE)
    }
    fp
  }) |> stats::setNames(names(df))
}

# ---------------------------------------------------------------------------
# Name-based and range-based heuristics. Kept tiny and explicit on purpose:
# every rule has a single regex or numeric range and is covered by a test.
# ---------------------------------------------------------------------------

.name_is_admin_index <- function(name) {
  grepl("^(row[._-]?num(ber)?|row[._-]?id|idx|index|obs[._-]?num(ber)?|n_obs)$",
        tolower(name))
}

.name_is_id <- function(name) {
  grepl("(^|[._-])(id|uuid|guid|key|code)([._-]|$)", tolower(name)) ||
    grepl("(plot|sample|site|subject|observation|station)id$", tolower(name))
}

.name_is_lat <- function(nm_lo) {
  grepl("^(lat|latitude|y_coord)$", nm_lo) || grepl("(^|_)lat(_|$)", nm_lo)
}

.name_is_lon <- function(nm_lo) {
  grepl("^(lon|lng|long|longitude|x_coord)$", nm_lo) ||
    grepl("(^|_)(lon|lng|long)(_|$)", nm_lo)
}

.range_is_lat <- function(vals) {
  if (length(vals) == 0L) return(FALSE)
  # Allow up to 1% of values outside the legal range — those will be flagged
  # as anomalies but do not disqualify the column from coord_lat.
  q <- stats::quantile(vals, c(0.01, 0.99))
  q[[1]] >= -90 && q[[2]] <= 90
}

.range_is_lon <- function(vals) {
  if (length(vals) == 0L) return(FALSE)
  q <- stats::quantile(vals, c(0.01, 0.99))
  q[[1]] >= -180 && q[[2]] <= 180
}

.is_year_like <- function(col, name, nrow_df) {
  if (!is.numeric(col)) return(FALSE)
  vals <- col[!is.na(col)]
  if (length(vals) == 0L) return(FALSE)
  if (any(vals != floor(vals))) return(FALSE)
  rng <- range(vals)
  if (rng[1] < 1500 || rng[2] > 2200) return(FALSE)
  nm_lo <- tolower(name)
  grepl("^(year|yr|sampling[._-]?year|obs[._-]?year)$", nm_lo)
}

.looks_compositional <- function(col, name) {
  vals <- col[!is.na(col)]
  if (length(vals) == 0L) return(FALSE)
  nm_lo <- tolower(name)
  if (!grepl("(cover|percent|pct|prop|proportion|share|fraction|rel[._-]?abund)",
             nm_lo)) return(FALSE)
  # Accept compositional if the bulk of the distribution sits in [0, 1] or
  # [0, 100]. Stragglers above 1.0 / 100 are tolerated and become anomalies.
  q <- stats::quantile(vals, c(0.01, 0.99))
  in01    <- q[[1]] >= -1e-6 && q[[2]] <= 1.05
  in0_100 <- q[[1]] >= -1e-6 && q[[2]] <= 105
  in01 || in0_100
}

# Roles excluded from numeric-numeric relationship screening.
# Coordinates and temporal columns participate in *suspicious* drift checks
# but are excluded from the symmetric numeric pair sweep.
.ignored_roles <- c("id", "admin_index", "group_id",
                    "near_constant", "constant",
                    "coord_lat", "coord_lon", "temporal",
                    "date", "unknown")

# Human-readable explanation for why a column is excluded from screening.
.role_ignore_reason <- function(role) {
  switch(role,
    id            = "it behaves like an identifier",
    admin_index   = "it behaves like an administrative index",
    group_id      = "it behaves like a grouping identifier",
    near_constant = "it carries almost no variation",
    constant      = "it is constant",
    temporal      = "it is a temporal column, screened separately as drift",
    coord_lat     = "it is a spatial coordinate, screened separately as drift",
    coord_lon     = "it is a spatial coordinate, screened separately as drift",
    date          = "it is a date column, screened separately as drift",
    unknown       = "its type was not recognised",
    NULL
  )
}

# Pretty role labels used in details() output.
.role_pretty <- function(role) {
  switch(role,
    id            = "identifier",
    admin_index   = "administrative index",
    group_id      = "grouping identifier",
    temporal      = "temporal",
    coord_lat     = "spatial coordinate (latitude)",
    coord_lon     = "spatial coordinate (longitude)",
    measurement   = "continuous",
    compositional = "compositional",
    categorical   = "categorical",
    flag          = "logical flag",
    near_constant = "near-constant",
    constant      = "constant",
    date          = "date",
    unknown       = "unknown",
    role
  )
}
