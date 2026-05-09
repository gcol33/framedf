#' Triage a data frame for structure, relationships, and anomalies
#'
#' @param data A data frame.
#' @param adjustment Character vector of column names to partial out before
#'   numeric-numeric screening. NULL (default) means no adjustment.
#' @param ... Override default settings: min_obs, strong_threshold,
#'   moderate_threshold, weak_threshold, near_constant_ratio,
#'   id_unique_ratio, skew_threshold.
#' @return An object of class `frame_df`.
#' @export
frame <- function(data, adjustment = NULL, ...) {
  if (!is.data.frame(data)) stop("`data` must be a data frame")

  settings   <- .default_settings(adjustment = adjustment, ...)
  primitives <- .build_primitives()

  col_fps  <- .fingerprint_columns(data)
  roles    <- .infer_roles(data, settings)
  rel      <- .screen_relationships(data, roles, settings, primitives)
  anomalies <- .detect_anomalies(data, roles, settings)

  structure(
    list(
      data_summary          = list(nrow = nrow(data), ncol = ncol(data),
                                   col_names = names(data)),
      column_fingerprints   = col_fps,
      roles                 = roles,
      structure             = .structure_summary(roles),
      relationship_findings = rel$findings,
      ignored_pairs         = rel$ignored_pairs,
      ignored_cols          = rel$ignored_cols,
      n_pairs_screened      = rel$n_pairs_screened,
      anomaly_findings      = anomalies,
      settings              = settings
    ),
    class = "frame_df"
  )
}

.default_settings <- function(...) {
  s <- list(
    min_obs             = 10L,
    strong_threshold    = 0.5,
    moderate_threshold  = 0.3,
    weak_threshold      = 0.1,
    near_constant_ratio = 0.05,
    id_unique_ratio     = 0.95,
    skew_threshold      = 1.0,
    adjustment          = NULL
  )
  args <- list(...)
  for (nm in names(args)) s[[nm]] <- args[[nm]]
  s
}

# Select C++ primitives when the shared library is loaded; fall back to R.
.build_primitives <- function() {
  use_cpp <- isTRUE(.framedf_env$use_cpp)
  if (use_cpp) {
    list(
      lm_simple     = lm_simple_cpp,
      lm_multiple   = .lm_multiple_r,    # C++ pending
      residualize   = residualize_cpp,
      group_summary = group_summary_cpp
    )
  } else {
    list(
      lm_simple     = .lm_simple_r,
      lm_multiple   = .lm_multiple_r,
      residualize   = .residualize_r,
      group_summary = .group_summary_r
    )
  }
}

.structure_summary <- function(roles) {
  list(
    n_measurement   = sum(roles == "measurement"),
    n_categorical   = sum(roles %in% c("categorical", "flag")),
    n_id            = sum(roles %in% c("id", "admin_index")),
    n_date          = sum(roles == "date"),
    n_constant      = sum(roles %in% c("constant", "near_constant")),
    n_coord         = sum(grepl("^coord_", roles)),
    n_unknown       = sum(roles == "unknown")
  )
}
