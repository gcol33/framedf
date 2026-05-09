# Show Anomaly Findings from `frame()`

Lists per-column anomalies grouped by qualitative pattern: implausible
values, totals exceeding bounds, capitalization inconsistency, rare
levels, distributional outliers, and skewed distributions.

## Usage

``` r
anomalies(x, ...)

# S3 method for class 'frame_df'
anomalies(x, ...)
```

## Arguments

- x:

  A `frame_df` object.

- ...:

  Additional arguments (unused).

## Value

The list of anomaly findings, invisibly.

## Details

Numeric backing detail (Tukey fences, skewness coefficients, indices of
offending rows) is available on the returned object; the print view is
meant to be skimmable.

## Examples

``` r
df <- data.frame(
  y = c(rnorm(98), 100, -100),
  g = c(rep("A", 49), rep("a", 49), "B", "B")
)
anomalies(frame(df))
```
