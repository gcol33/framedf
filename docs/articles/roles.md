# Roles and Rules

`framedf` decides what to do with a column based on the **role** it
infers. This vignette documents every role, the cues used to assign it,
and the screening behaviour it triggers.

## Why Roles Matter

A correlation matrix treats `row_id` and `temperature` identically. They
are both numeric, after all. But conceptually they are not the same
column — `row_id` is bookkeeping, and a strong correlation between
`row_id` and `temperature` is almost always a sign that the data is
sorted by time, not a real association.

Role inference is what lets `framedf` say *“latitude was ignored because
it is a spatial coordinate, screened separately as drift”* instead of
just dumping a Pearson coefficient.

## The Roles

| Role | Cues used to assign it | Screening behaviour |
|----|----|----|
| `id` | All-unique integer with id-like name; or character with very high uniqueness ratio. | Excluded from the numeric pair sweep. Listed in **Ignored**. |
| `admin_index` | Sequential integer with a row-number-like name. | Excluded. |
| `group_id` | Repeated integer or repeated character with id-like name. | Excluded from the symmetric numeric sweep, but used as a *grouping* in cat–num scan. |
| `temporal` | Date or POSIX class; or integer in 1500–2200 with a year-like name. | Excluded from the symmetric sweep; participates in the drift screen. |
| `coord_lat` | Name matches `lat`/`latitude` and the bulk of values fall within `[-90, 90]`. | Excluded; participates in the drift screen. |
| `coord_lon` | Name matches `lon`/`lng`/`longitude` and the bulk of values fall within `[-180, 180]`. | Excluded; participates in the drift screen. |
| `measurement` | Continuous numeric without any other cue. | Screened symmetrically against all other measurements (and compositionals). |
| `compositional` | Numeric with a name like `*cover*`, `*pct*`, `*share*`, `*proportion*`, in `[0, 1]` or `[0, 100]`. | Screened, plus checked pairwise for constrained-complement structure. |
| `categorical` | Character or factor with low cardinality. | Used as a grouping in the cat–num scan. |
| `flag` | Logical. | Treated like a categorical with two levels. |
| `near_constant` | Dominant value covers more than `(1 - near_constant_ratio)` of rows. | Excluded. |
| `constant` | Only one unique value. | Excluded. |
| `unknown` | None of the above. | Excluded. |

## Worked Example

``` r

df <- data.frame(
  PlotID            = sample(1:50, 200, replace = TRUE),
  PlotObservationID = seq_len(200L),
  row_number        = seq_len(200L),
  year              = sample(1990:2020, 200, replace = TRUE),
  latitude          = stats::runif(200, 40, 50),
  longitude         = stats::runif(200, 5, 15),
  elevation         = stats::runif(200, 0, 2500),
  observer_id       = sample(letters[1:5], 200, replace = TRUE),
  country           = sample(c("AT", "DE", "CH"), 200, replace = TRUE),
  native_cover      = stats::runif(200, 0, 1),
  flagged           = sample(c(TRUE, FALSE), 200, replace = TRUE),
  stringsAsFactors  = FALSE
)
df$alien_cover <- 1 - df$native_cover
fd <- frame(df)
details(fd)
#> Details
#> 
#> Analysis mode
#> ────────────────
#> relationship screening used the full data
#> all pairs above the minimum complete-case count were screened directly
#> 
#> Column roles
#> ────────────────
#> PlotID: grouping identifier
#> PlotObservationID: identifier
#> row_number: administrative index
#> year: temporal
#> latitude: spatial coordinate (latitude)
#> longitude: spatial coordinate (longitude)
#> elevation: continuous
#> observer_id: grouping identifier
#> country: categorical
#> native_cover: compositional
#> flagged: logical flag
#> alien_cover: compositional
#> 
#> Skipped relationship rules
#> ────────────────
#> identifier × anything: skipped
#> administrative index × anything: skipped
#> near-constant or constant × anything: skipped
#> temporal × measurement: not screened symmetrically; checked as drift instead
#> coordinate × measurement: not screened symmetrically; checked as drift instead
#> many-level grouping × continuous: screened, but flagged as observer-style if strong
#> 
#> Backend
#> ────────────────
#> primitives: C++ (Rcpp)
#> numeric numeric pairs: screened by ordinary least squares (with QR residualisation when adjusted)
#> categorical numeric pairs: one-way analysis-of-variance summaries, eta squared as effect size
#> compositional pairs: pairwise sum stability under coefficient of variation
#> drift checks: simple linear fits of spatial coordinates against time
```

The **Column roles** block in
[`details()`](https://gillescolling.com/framedf/reference/details.md)
lists every column with its inferred role. Anything in the **Ignored**
section of `print(fd)` will also be there.

## Tuning the Rules

Every threshold is a parameter:

``` r

frame(df,
      id_unique_ratio     = 0.99,   # require 99% unique to call a string an id
      near_constant_ratio = 0.01,   # call near-constant if dominant > 99%
      strong_threshold    = 0.6,
      moderate_threshold  = 0.4,
      weak_threshold      = 0.15,
      compositional_cv    = 0.05,
      observer_min_levels = 4L,
      min_level_n         = 5L)
```

[`framedf_settings()`](https://gillescolling.com/framedf/reference/framedf_settings.md)
returns the full default list with documentation for every field.

## When the Role Is Wrong

Role inference is heuristic, and edge cases will land in the wrong
bucket sometimes. Two ways to recover:

1.  Rename the column. Names like `*_id`, `*_year`, `*_lat`, `*_cover`
    are picked up immediately.
2.  Read the role from `fd$roles` and override the analysis manually:

``` r

fd <- frame(df)
fd$roles[["my_weird_column"]] <- "measurement"
# (re-run any reader you need on the modified object)
```

The triage object is a plain list, so post-hoc edits are safe. For
production use, prefer fixing the column name or extending the package’s
name-based heuristics.
