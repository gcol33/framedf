# Higher-level structural summary of a data frame, derived from inferred
# roles. Drives the "Structure" section of print.frame_df.
#
# What we look for:
# - temporal structure (date / year columns)
# - spatial structure (lat / lon columns)
# - likely identifiers (column-by-column id / admin_index labels)
# - grouping structure (repeated values within an id-like column,
#   or membership in a low-cardinality categorical)
# - dataframe shape: cross-sectional, repeated-measure, longitudinal, ...

.summarise_structure <- function(df, roles, fps) {
  temporal  <- names(roles)[roles == "temporal"]
  spatial   <- names(roles)[roles %in% c("coord_lat", "coord_lon")]
  # Identifiers presented to the user combine `id` columns and
  # grouping ids; admin_index is intentionally not surfaced here because
  # readers usually think of row numbers as bookkeeping, not as identifying
  # information.
  ids       <- names(roles)[roles == "id"]
  group_ids <- names(roles)[roles == "group_id"]
  cats      <- names(roles)[roles == "categorical"]

  # Detect grouping membership: a categorical column with low cardinality
  # and a name that suggests a partition (country, region, site, group).
  group_cats <- cats[vapply(cats, function(nm) {
    fp <- fps[[nm]]
    n_uniq <- fp$n_unique
    if (is.null(n_uniq)) return(FALSE)
    n_uniq >= 2L && n_uniq <= 50L && .name_is_partition(nm)
  }, logical(1L))]

  # Repeated-measure detection: a group_id column with non-unique values
  # AND a temporal column → repeated measures within the id, over time.
  repeated_within <- character(0)
  for (g in group_ids) {
    n_uniq <- fps[[g]]$n_unique
    if (!is.null(n_uniq) && n_uniq < nrow(df)) {
      repeated_within <- c(repeated_within, g)
    }
  }

  shape <- .classify_shape(
    n          = nrow(df),
    has_time   = length(temporal) > 0,
    has_repeat = length(repeated_within) > 0,
    has_space  = length(spatial) > 0
  )

  list(
    shape           = shape,
    temporal        = temporal,
    spatial         = spatial,
    identifiers     = ids,
    group_ids       = group_ids,
    grouping_cats   = group_cats,
    repeated_within = repeated_within
  )
}

.classify_shape <- function(n, has_time, has_repeat, has_space) {
  if (has_repeat && has_time)            return("repeated_measure_observational")
  if (has_repeat)                        return("grouped_observational")
  if (has_time && has_space)             return("spatiotemporal_observational")
  if (has_time)                          return("temporal_observational")
  if (has_space)                         return("spatial_observational")
  "cross_sectional"
}

.shape_phrase <- function(shape) {
  switch(shape,
    repeated_measure_observational = "Looks like a repeated-measure observational dataframe.",
    grouped_observational          = "Looks like a grouped observational dataframe.",
    spatiotemporal_observational   = "Looks like a spatially and temporally indexed observational dataframe.",
    temporal_observational         = "Looks like a temporally indexed observational dataframe.",
    spatial_observational          = "Looks like a spatially indexed observational dataframe.",
    cross_sectional                = "Looks like a flat cross-sectional dataframe.",
    "Could not classify the broad shape of this dataframe."
  )
}

.name_is_partition <- function(name) {
  nm_lo <- tolower(name)
  grepl(paste0(
    "(^|[._-])(country|region|state|province|county|district|",
    "site|location|area|zone|biome|habitat|group|class|category|",
    "treatment|species|taxon|genus|family|order|class|phylum)([._-]|$)"
  ), nm_lo)
}
