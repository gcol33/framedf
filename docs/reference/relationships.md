# Show Relationships Found by `frame()`

Prints a structured listing of all relationship findings, grouped into
four kinds: meaningful, suspicious, structural, and ignored. Each pair
is described qualitatively (direction, strength, stability, method).

## Usage

``` r
relationships(x, ...)

# S3 method for class 'frame_df'
relationships(x, min_strength = "weak", kind = NULL, ...)
```

## Arguments

- x:

  A `frame_df` object.

- ...:

  Additional arguments (unused).

- min_strength:

  Minimum strength to display: `"weak"`, `"moderate"`, or `"strong"`.
  Structural relationships are always shown.

- kind:

  Restrict to a single kind: `"meaningful"`, `"suspicious"`,
  `"structural"`, `"ignored"`, or `NULL` (all).

## Value

The list of findings, invisibly.

## Details

Numeric evidence (correlation coefficients, F-statistics, p-values) is
available on the returned object; the printed view is meant to be
skimmable.

## Examples

``` r
set.seed(1)
df <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)
df$z <- 0.8 * df$x + 0.2 * stats::rnorm(100)
relationships(frame(df))
```
