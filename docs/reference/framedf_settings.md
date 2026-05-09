# Settings for `frame()`

Returns the list of tunable thresholds used by
[`frame()`](https://gillescolling.com/framedf/reference/frame.md). Every
field can be overridden through the `...` argument of
[`frame()`](https://gillescolling.com/framedf/reference/frame.md). The
defaults are chosen to be conservative on small data and to scale to
millions of rows through progressive subsampling.

## Usage

``` r
framedf_settings(adjustment = NULL, ...)
```

## Arguments

- adjustment:

  Optional adjustment columns (passed through unchanged).

- ...:

  Named overrides.

## Value

A named list of settings.

## Fields

- `min_obs`:

  Minimum complete cases required to screen a pair.

- `strong_threshold`, `moderate_threshold`, `weak_threshold`:

  Absolute correlation cut-offs used to classify pair strength.

- `near_constant_ratio`:

  Maximum allowed share of the dominant value before a column is flagged
  as near-constant.

- `id_unique_ratio`:

  If a character column has more than this share of unique values, it is
  treated as an identifier.

- `skew_threshold`:

  Absolute skewness above which a column is flagged as right- or
  left-skewed.

- `subsample_threshold`:

  Row count above which numeric pairs are screened by progressive
  subsampling.

- `subsample_probe`, `subsample_confirm`:

  Probe and confirmation sample sizes for the two-stage subsampling.

- `compositional_cv`:

  Maximum coefficient of variation of `x + y` for the pair to be flagged
  as a constrained complement.

- `observer_min_levels`:

  Minimum number of categorical levels before a strong group effect
  counts as an observer-style concern.

- `min_level_n`:

  Levels with fewer than this many rows are flagged as rare.

- `seed`:

  Seed used by the subsampling layer.

- `adjustment`:

  Optional column names to partial out before numeric–numeric screening.
