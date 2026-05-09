#' framedf: Calm Triage of Unfamiliar Data Frames
#'
#' `framedf` looks at a data frame the way an experienced analyst does in
#' the first thirty seconds: it infers what each column means, screens
#' every sensible pair for relationships, and lists the anomalies worth
#' reading first. The output is qualitative — direction, strength,
#' stability — not raw test statistics.
#'
#' @section Entry points:
#' \describe{
#'   \item{[frame()]}{build a triage object from a data frame.}
#'   \item{[print.frame_df()]}{narrative overview.}
#'   \item{[relationships()]}{meaningful, suspicious, structural, ignored pairs.}
#'   \item{[anomalies()]}{per-column oddities.}
#'   \item{[details()]}{analysis mode, roles, skipped rules, backend.}
#'   \item{[framedf_settings()]}{tunable thresholds.}
#' }
#'
#' @keywords internal
#' @name framedf-package
#' @aliases framedf
#' @useDynLib framedf, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats complete.cases median pf pt quantile sd setNames
#' @importFrom utils combn head
"_PACKAGE"
