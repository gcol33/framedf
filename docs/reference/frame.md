# Triage a Data Frame for Structure, Relationships, and Anomalies

`frame()` reads a data frame, infers a semantic role for each column
(identifier, temporal, spatial, categorical, continuous, ...), screens
all sensible pairs of columns for relationships, and surfaces a small
set of anomalies in a single pass. The result is a calm, qualitative
summary suitable for the first thirty seconds of working with an
unfamiliar dataset, not a model fit.

## Usage

``` r
frame(data, adjustment = NULL, ...)
```

## Arguments

- data:

  A data frame.

- adjustment:

  Optional character vector of column names to partial out (residualize
  against) before numeric–numeric screening. Useful when one variable is
  suspected of confounding most pairs.

- ...:

  Optional overrides for screening settings. See
  [`framedf_settings()`](https://gillescolling.com/framedf/reference/framedf_settings.md)
  for the full list with defaults.

## Value

An object of class `"frame_df"` containing role assignments, structure
summary, relationship findings, ignored pairs, anomaly findings, and the
settings used.

## Details

Three reader functions consume the result:

- [`print.frame_df()`](https://gillescolling.com/framedf/reference/print.frame_df.md)
  — narrative overview (the default `print(frame(df))`)

- [`relationships()`](https://gillescolling.com/framedf/reference/relationships.md)
  — meaningful, suspicious, structural, and ignored pairs

- [`anomalies()`](https://gillescolling.com/framedf/reference/anomalies.md)
  — per-column oddities (range, distribution, capitalization)

- [`details()`](https://gillescolling.com/framedf/reference/details.md)
  — analysis mode, column roles, skipped rules, backend

## Examples

``` r
set.seed(1)
n <- 200
df <- data.frame(
  plot_id    = sample(1:40, n, replace = TRUE),
  year       = sample(2010:2020, n, replace = TRUE),
  latitude   = runif(n, 40, 50),
  longitude  = runif(n, 5, 15),
  elevation  = runif(n, 0, 2500),
  temperature = NA_real_,
  richness   = rpois(n, 30),
  stringsAsFactors = FALSE
)
df$temperature <- 20 - df$elevation / 200 + stats::rnorm(n)
fd <- frame(df)
print(fd)
```
