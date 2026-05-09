#' Triage a Data Frame for Structure, Relationships, and Anomalies
#'
#' `frame()` reads a data frame, infers a semantic role for each column
#' (identifier, temporal, spatial, categorical, continuous, ...), screens
#' all sensible pairs of columns for relationships, and surfaces a small
#' set of anomalies in a single pass. The result is a qualitative summary
#' suitable for the first thirty seconds of working with an unfamiliar
#' dataset.
#'
#' Reader functions consume the result:
#'
#' * [print.frame_df()]: narrative overview (the default `print(frame(df))`)
#' * [relationships()]: meaningful, suspicious, structural, and ignored pairs
#' * [anomalies()]: per-column oddities (range, distribution, capitalization)
#' * [details()]: analysis mode, column roles, skipped rules, backend
#'
#' @param data A data frame.
#' @param adjustment Optional character vector of column names to partial
#'   out (residualize against) before numeric–numeric screening. Useful when
#'   one variable is suspected of confounding most pairs.
#' @param ... Optional overrides for screening settings. See
#'   [framedf_settings()] for the full list with defaults.
#'
#' @return An object of class `"frame_df"` containing role assignments,
#'   structure summary, relationship findings, ignored pairs, anomaly
#'   findings, and the settings used.
#'
#' @examples
#' set.seed(1)
#' n <- 200
#' df <- data.frame(
#'   plot_id    = sample(1:40, n, replace = TRUE),
#'   year       = sample(2010:2020, n, replace = TRUE),
#'   latitude   = runif(n, 40, 50),
#'   longitude  = runif(n, 5, 15),
#'   elevation  = runif(n, 0, 2500),
#'   temperature = NA_real_,
#'   richness   = rpois(n, 30),
#'   stringsAsFactors = FALSE
#' )
#' df$temperature <- 20 - df$elevation / 200 + stats::rnorm(n)
#' fd <- frame(df)
#' print(fd)
#'
#' @export
frame <- function(data, adjustment = NULL, ...) {
  if (!is.data.frame(data)) stop("`data` must be a data frame")

  settings   <- framedf_settings(adjustment = adjustment, ...)
  primitives <- .build_primitives()

  fps        <- .fingerprint_columns(data)
  roles      <- .infer_roles(data, settings)
  structure_ <- .summarise_structure(data, roles, fps)
  rel        <- .screen_relationships(data, roles, fps, settings, primitives)
  anomalies  <- .detect_anomalies(data, roles, fps, settings)
  missing_   <- .detect_missingness(data, roles, fps, settings)
  inflation_ <- .detect_inflation(data, roles, fps, settings)

  structure(
    list(
      data_summary          = list(
        nrow      = nrow(data),
        ncol      = ncol(data),
        col_names = names(data)
      ),
      column_fingerprints   = fps,
      roles                 = roles,
      structure             = structure_,
      relationship_findings = rel$findings,
      ignored_pairs         = rel$ignored_pairs,
      ignored_cols          = rel$ignored_cols,
      n_pairs_screened      = rel$n_pairs_screened,
      anomaly_findings      = anomalies,
      missingness_findings  = missing_,
      inflation_findings    = inflation_,
      settings              = settings
    ),
    class = "frame_df"
  )
}

#' Settings for `frame()`
#'
#' Returns the list of tunable thresholds used by [frame()]. Every field can
#' be overridden through the `...` argument of `frame()`. The defaults are
#' chosen to be conservative on small data and to scale to millions of rows
#' through progressive subsampling.
#'
#' @param adjustment Optional adjustment columns (passed through unchanged).
#' @param ... Named overrides.
#'
#' @section Fields:
#' \describe{
#'   \item{`min_obs`}{Minimum complete cases required to screen a pair.}
#'   \item{`strong_threshold`, `moderate_threshold`, `weak_threshold`}{
#'     Absolute correlation cut-offs used to classify pair strength.}
#'   \item{`near_constant_ratio`}{Maximum allowed share of the dominant value
#'     before a column is flagged as near-constant.}
#'   \item{`id_unique_ratio`}{If a character column has more than this share
#'     of unique values, it is treated as an identifier.}
#'   \item{`skew_threshold`}{Absolute skewness above which a column is
#'     flagged as right- or left-skewed.}
#'   \item{`subsample_threshold`}{Row count above which numeric pairs are
#'     screened by progressive subsampling.}
#'   \item{`subsample_probe`, `subsample_confirm`}{Probe and confirmation
#'     sample sizes for the two-stage subsampling.}
#'   \item{`compositional_cv`}{Maximum coefficient of variation of `x + y`
#'     for the pair to be flagged as a constrained complement.}
#'   \item{`observer_min_levels`}{Minimum number of categorical levels
#'     before a strong group effect counts as an observer-style concern.}
#'   \item{`min_level_n`}{Levels with fewer than this many rows are flagged
#'     as rare.}
#'   \item{`seed`}{Seed used by the subsampling layer.}
#'   \item{`adjustment`}{Optional column names to partial out before
#'     numeric pair screening.}
#' }
#' @return A named list of settings.
#' @export
framedf_settings <- function(adjustment = NULL, ...) {
  s <- list(
    min_obs              = 10L,
    strong_threshold     = 0.5,
    moderate_threshold   = 0.3,
    weak_threshold       = 0.1,
    near_constant_ratio  = 0.05,
    id_unique_ratio      = 0.95,
    skew_threshold       = 1.0,
    subsample_threshold  = 50000L,
    subsample_probe      = 5000L,
    subsample_confirm    = 50000L,
    compositional_cv     = 0.02,
    observer_min_levels  = 5L,
    min_level_n          = 3L,
    seed                 = 1L,
    adjustment           = adjustment
  )
  args <- list(...)
  for (nm in names(args)) s[[nm]] <- args[[nm]]
  s
}

# Choose C++ primitives when the shared library is loaded; fall back to R.
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
