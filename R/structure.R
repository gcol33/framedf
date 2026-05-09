# Higher-level structural summary of a data frame, derived from inferred
# roles. Drives the "Structure" section of print.frame_df.
#
# What we look for:
# - temporal structure (date / year columns)
# - spatial structure (lat / lon columns)
# - likely identifiers (column-by-column id / admin_index labels)
# - grouping structure (repeated values within an id-like column,
#   or membership in a low-cardinality categorical)
# - nestedness (A nested in B: each A maps to one B)
# - observation unit (the most specific identifier of a row)
# - compositional groups (sets of compositional columns that together
#   describe a partition)
# - dataframe shape: cross-sectional, repeated-measure, longitudinal, ...

.summarise_structure <- function(df, roles, fps) {
  temporal  <- names(roles)[roles == "temporal"]
  spatial   <- names(roles)[roles %in% c("coord_lat", "coord_lon")]
  ids       <- names(roles)[roles == "id"]
  group_ids <- names(roles)[roles == "group_id"]
  cats      <- names(roles)[roles == "categorical"]

  group_cats <- cats[vapply(cats, function(nm) {
    fp <- fps[[nm]]
    n_uniq <- fp$n_unique
    if (is.null(n_uniq)) return(FALSE)
    n_uniq >= 2L && n_uniq <= 50L && .name_is_partition(nm)
  }, logical(1L))]

  repeated_within <- character(0)
  for (g in group_ids) {
    n_uniq <- fps[[g]]$n_unique
    if (!is.null(n_uniq) && n_uniq < nrow(df)) {
      repeated_within <- c(repeated_within, g)
    }
  }

  obs_unit <- .detect_observation_unit(df, roles, fps)
  nested   <- .detect_nestedness(df, roles, fps, ids, group_ids,
                                 group_cats, spatial)
  comp_grp <- names(roles)[roles == "compositional"]

  shape <- .classify_shape(
    n          = nrow(df),
    has_time   = length(temporal) > 0,
    has_repeat = length(repeated_within) > 0,
    has_space  = length(spatial) > 0
  )

  list(
    shape              = shape,
    temporal           = temporal,
    spatial            = spatial,
    identifiers        = ids,
    group_ids          = group_ids,
    grouping_cats      = group_cats,
    repeated_within    = repeated_within,
    observation_unit   = obs_unit,
    nested             = nested,
    compositional_cols = comp_grp
  )
}

.classify_shape <- function(n, has_time, has_repeat, has_space) {
  if (has_repeat && has_time && has_space) return("spatial_repeated_measure")
  if (has_repeat && has_time)              return("repeated_measure_observational")
  if (has_repeat)                          return("grouped_observational")
  if (has_time && has_space)               return("spatiotemporal_observational")
  if (has_time)                            return("temporal_observational")
  if (has_space)                           return("spatial_observational")
  "cross_sectional"
}

.shape_phrase <- function(shape) {
  switch(shape,
    spatial_repeated_measure       = "Looks like a spatial repeated-measure observational dataframe.",
    repeated_measure_observational = "Looks like a repeated-measure observational dataframe.",
    grouped_observational          = "Looks like a grouped observational dataframe.",
    spatiotemporal_observational   = "Looks like a spatially and temporally indexed observational dataframe.",
    temporal_observational         = "Looks like a temporally indexed observational dataframe.",
    spatial_observational          = "Looks like a spatially indexed observational dataframe.",
    cross_sectional                = "Looks like a flat cross-sectional dataframe.",
    "Could not classify the broad shape of this dataframe."
  )
}

# Observation unit: the per-row identifier. We pick the id-typed column
# whose value uniquely identifies a row, preferring names that look like an
# observation unit (PlotObservationID, observation_id, plot_obs).
.detect_observation_unit <- function(df, roles, fps) {
  ids <- names(roles)[roles == "id"]
  if (length(ids) == 0L) return(NULL)
  unique_ids <- ids[vapply(ids, function(nm) {
    fp <- fps[[nm]]
    !is.null(fp$n_unique) && fp$n_unique == nrow(df)
  }, logical(1L))]
  if (length(unique_ids) == 0L) return(NULL)
  obs_like <- unique_ids[grepl("(observation|obs)", tolower(unique_ids))]
  pick <- if (length(obs_like) > 0L) obs_like[[1L]] else unique_ids[[1L]]
  .pretty_observation_unit(pick)
}

.pretty_observation_unit <- function(nm) {
  nm_lo <- tolower(nm)
  if (grepl("plot.*obs", nm_lo))      return("plot observation")
  if (grepl("obs.*plot", nm_lo))      return("plot observation")
  if (grepl("plot", nm_lo))           return("plot")
  if (grepl("sample", nm_lo))         return("sample")
  if (grepl("observation|obs", nm_lo)) return("observation")
  if (grepl("subject", nm_lo))        return("subject")
  if (grepl("site", nm_lo))           return("site")
  "observation"
}

# Nestedness: A nested in B if each value of A maps to one value of B.
# We check candidates among id, group_id, partition-categorical, and
# spatial/admin grouping columns. Cheap exact check using a tabulation;
# downsamples for very large data.
.detect_nestedness <- function(df, roles, fps, ids, group_ids,
                               group_cats, spatial) {
  cands <- unique(c(ids, group_ids, group_cats))
  if (length(cands) < 2L) return(list())

  n <- nrow(df)
  rows <- if (n > 50000L) sample.int(n, 50000L) else seq_len(n)
  d <- df[rows, cands, drop = FALSE]

  out <- list()
  pairs <- utils::combn(cands, 2L, simplify = FALSE)
  for (pair in pairs) {
    a_nm <- pair[[1L]]; b_nm <- pair[[2L]]
    a <- d[[a_nm]]; b <- d[[b_nm]]
    ok <- !is.na(a) & !is.na(b)
    if (sum(ok) < 10L) next
    a <- a[ok]; b <- b[ok]
    a_uniq_for_b <- length(unique(a)) > length(unique(b))

    nest_ab <- .is_nested(a, b)  # A nested in B
    nest_ba <- .is_nested(b, a)  # B nested in A

    if (nest_ab && !nest_ba && a_uniq_for_b) {
      out[[length(out) + 1L]] <- list(child = a_nm, parent = b_nm)
    } else if (nest_ba && !nest_ab && !a_uniq_for_b) {
      out[[length(out) + 1L]] <- list(child = b_nm, parent = a_nm)
    }
  }
  out
}

# A nested in B: each unique value of A appears with at most one value
# of B (modulo a tiny tolerance for noise).
.is_nested <- function(a, b) {
  ua <- unique(a)
  if (length(ua) <= 1L) return(FALSE)
  if (length(ua) == length(a)) return(FALSE)
  for (av in ua) {
    bs <- unique(b[a == av])
    if (length(bs) > 1L) return(FALSE)
  }
  TRUE
}

.name_is_partition <- function(name) {
  nm_lo <- tolower(name)
  grepl(paste0(
    "(^|[._-])(country|region|state|province|county|district|",
    "site|location|area|zone|biome|habitat|group|class|category|",
    "treatment|species|taxon|genus|family|order|class|phylum)([._-]|$)"
  ), nm_lo)
}
