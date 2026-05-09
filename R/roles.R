# Role inference: assign a semantic role to each column.
# Roles drive which columns are screened and what language is used when
# explaining ignored columns.

.infer_roles <- function(df, settings) {
  nrow_df <- nrow(df)
  roles   <- character(ncol(df))
  names(roles) <- names(df)
  for (nm in names(df)) {
    roles[[nm]] <- .classify_column(df[[nm]], nm, nrow_df, settings)
  }
  roles
}

.classify_column <- function(col, name, nrow_df, settings) {
  if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) return("date")
  if (is.logical(col)) return("flag")

  if (is.numeric(col)) {
    vals    <- col[!is.na(col)]
    n_valid <- length(vals)
    n_uniq  <- length(unique(vals))

    if (n_uniq <= 1L) return("constant")

    # Near-constant: the dominant value covers > (1 - near_constant_ratio) of rows.
    if (n_valid > 0) {
      dom_freq <- max(tabulate(match(vals, unique(vals)))) / n_valid
      if (dom_freq > 1 - settings$near_constant_ratio) return("near_constant")
    }

    # Sequential administrative index: integers named row_number / index / order etc.
    if (is.integer(col) && n_uniq == nrow_df && .name_is_admin_index(name)) {
      return("admin_index")
    }

    # General identifier: all unique, integer-typed
    if (n_uniq == nrow_df && is.integer(col)) return("id")

    nm_lo <- tolower(name)
    if (grepl("^(lat|latitude)$",    nm_lo)) return("coord_lat")
    if (grepl("^(lon|lng|longitude)$", nm_lo)) return("coord_lon")

    return("measurement")
  }

  if (is.character(col) || is.factor(col)) {
    vals    <- if (is.factor(col)) levels(col) else unique(col[!is.na(col)])
    n_valid <- sum(!is.na(col))
    n_uniq  <- length(vals)

    # High-cardinality strings → identifier
    if (n_valid > 0 && n_uniq / n_valid > settings$id_unique_ratio) return("id")

    return("categorical")
  }

  "unknown"
}

.name_is_admin_index <- function(name) {
  grepl("^(row[._]?num(ber)?|row[._]?id|idx|index|obs[._]?num(ber)?|[._]?n|obs)$",
        tolower(name))
}

# Per-column descriptive statistics stored for later display.
.fingerprint_columns <- function(df) {
  lapply(names(df), function(nm) {
    col     <- df[[nm]]
    n_valid <- sum(!is.na(col))
    n_miss  <- sum(is.na(col))
    n_uniq  <- length(unique(col[!is.na(col)]))

    fp <- list(name = nm, class = class(col),
               n_valid = n_valid, n_miss = n_miss, n_unique = n_uniq)

    if (is.numeric(col) && n_valid > 0) {
      fp$mean   <- mean(col, na.rm = TRUE)
      fp$sd     <- sd(col,   na.rm = TRUE)
      fp$min    <- min(col,  na.rm = TRUE)
      fp$max    <- max(col,  na.rm = TRUE)
      fp$median <- median(col, na.rm = TRUE)
    }
    fp
  }) |> setNames(names(df))
}

# Human-readable explanation for why a column was not screened.
.role_ignore_reason <- function(role) {
  switch(role,
    id           = "it behaves like an identifier",
    admin_index  = "it behaves like an administrative index",
    near_constant = "it carries almost no variation",
    constant     = "it is constant",
    date         = "it is a date column",
    coord_lat    = "it is a coordinate (latitude)",
    coord_lon    = "it is a coordinate (longitude)",
    unknown      = "its type was not recognised",
    NULL
  )
}

# Roles that are excluded from relationship screening.
.ignored_roles <- c("id", "admin_index", "near_constant", "constant",
                    "date", "coord_lat", "coord_lon", "unknown")
