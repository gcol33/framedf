# Quick Start

## What `framedf` Does

`framedf` is a first-pass diagnostic for an unfamiliar data frame. It
reads the data the way a careful colleague would: by figuring out what
each column means, by noticing pairs of columns that move together, and
by flagging values that look wrong.

The result is qualitative. You get sentences like *“temperature
decreases strongly with elevation”*. Numeric evidence (Pearson
coefficients, F-statistics, p-values) is still on the object for anyone
who wants it; the printed view stays skimmable.

There is one entry point,
[`frame()`](https://gillescolling.com/framedf/reference/frame.md), and
four reader functions:

- `print(frame(df))`: narrative overview.

- `relationships(frame(df))`: pairs grouped as meaningful, suspicious,
  structural, or ignored.

- `anomalies(frame(df))`: per-column oddities.

- `details(frame(df))`: methods, roles, skipped rules, backend.

## Installation

``` r

# install.packages("pak")
pak::pak("gcol33/framedf")
```

## A First Look

``` r

set.seed(1)
n <- 300L
df <- data.frame(
  plot_id    = sample(1:60, n, replace = TRUE),
  year       = sample(2010:2020, n, replace = TRUE),
  latitude   = stats::runif(n, 40, 50),
  longitude  = stats::runif(n, 5, 15),
  elevation  = stats::runif(n, 0, 2500),
  observer   = sample(letters[1:6], n, replace = TRUE),
  country    = sample(c("AT", "DE", "CH"), n, replace = TRUE),
  richness   = NA_real_,
  cover_a    = stats::runif(n, 0, 1),
  stringsAsFactors = FALSE
)
df$temperature <- 20 - df$elevation / 200 + stats::rnorm(n)
df$richness    <- 5 + 0.4 * sqrt(stats::runif(n, 1, 100)) +
                  2 * (df$observer == "a") + stats::rnorm(n)
df$cover_b     <- 1 - df$cover_a
```

``` r

fd <- frame(df)
print(fd)
#> framedf
#> 
#> 300 rows × 11 columns
#> 
#> Structure
#> ────────────────
#> Looks like a spatial repeated-measure observational dataframe.
#> 
#> Detected temporal structure:
#> • year
#> 
#> Detected spatial structure:
#> • latitude
#> • longitude
#> 
#> Likely identifiers:
#> • plot_id
#> 
#> Possible grouping structure:
#> • repeated observations within plot_id
#> • observations grouped by country
#> 
#> Possible compositional structure:
#> • cover_a
#> • cover_b
#> 
#> 
#> Relationships
#> ────────────────
#> temperature strongly decreases with elevation
#> 
#> cover_b strongly decreases with cover_a
#> 
#> observer identity appears to influence richness estimates
#> 
#> plot_id appears to structure both elevation, richness, cover_a, temperature, and cover_b
#>   possible dataset effect
#> 
#> cover_a and cover_b behave as constrained complements
#> 
#> Anomalies
#> ────────────────
#> richness contains extreme values relative to most observations
#> 
#> Ignored relationships
#> ────────────────
#> plot_id was ignored because it behaves like a grouping identifier
#> year was ignored because it is a temporal column, screened separately as drift
#> latitude was ignored because it is a spatial coordinate, screened separately as drift
#> longitude was ignored because it is a spatial coordinate, screened separately as drift
```

The four sections of the print output map onto the four parts of any
first-look conversation:

- **Structure**: what kind of dataframe is this? How is it shaped, what
  identifies a row, what is the temporal/spatial axis?

- **Relationships**: what moves with what? Which patterns are real and
  which feel like artefacts?

- **Anomalies**: what looks wrong?

- **Ignored**: what was excluded from screening, and why?

## Drill Down

``` r

relationships(fd)
#> Relationships
#> 
#> meaningful
#> ────────────────
#> temperature ~ elevation
#>   direction: negative
#>   strength: strong
#>   stability: high
#>   method: numeric screening
#> 
#> cover_b ~ cover_a
#>   direction: negative
#>   strength: strong
#>   stability: high
#>   method: numeric screening
#> 
#> richness ~ observer
#>   pattern: group effect
#>   strength: strong
#>   stability: high
#>   method: categorical numeric screen
#> 
#> suspicious
#> ────────────────
#> elevation,richness,cover_a,temperature,cover_b ~ plot_id
#> 
#> structural
#> ────────────────
#> cover_b ~ cover_a
#>   pattern: constrained complement
#>   concern: compositional relationship
#>   method: compositional sum check
#> 
#> ignored
#> ────────────────
#> plot_id ~ elevation
#>   reason: it behaves like a grouping identifier
#> 
#> year ~ elevation
#>   reason: it is a temporal column, screened separately as drift
#> 
#> latitude ~ elevation
#>   reason: it is a spatial coordinate, screened separately as drift
#> 
#> longitude ~ elevation
#>   reason: it is a spatial coordinate, screened separately as drift
```

``` r

anomalies(fd)
#> Anomalies
#> 
#> outliers
#> ────────────────
#> richness
#>   pattern: outliers (Tukey fence)
#>   count: 3
#>   fence: [3.43923, 12.6733]
```

``` r

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
#> plot_id: grouping identifier
#> year: temporal
#> latitude: spatial coordinate (latitude)
#> longitude: spatial coordinate (longitude)
#> elevation: continuous
#> observer: categorical
#> country: categorical
#> richness: continuous
#> cover_a: compositional
#> temperature: continuous
#> cover_b: compositional
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

[`details()`](https://gillescolling.com/framedf/reference/details.md) is
the place to go when an output surprises you. It tells you which mode
the screening ran in, what role each column was given, which rules
caused some pairs to be skipped, and whether the C++ backend was used.

## Adjust for a Confounder

If you suspect one variable is confounding most of the others, you can
partial it out before screening:

``` r

fd_adj <- frame(df, adjustment = "elevation")
relationships(fd_adj, kind = "meaningful")
#> Relationships
#> 
#> meaningful
#> ────────────────
#> cover_b ~ cover_a
#>   direction: negative
#>   strength: strong
#>   stability: high
#>   method: numeric screening (adjusted)
#> 
#> richness ~ observer
#>   pattern: group effect
#>   strength: strong
#>   stability: high
#>   method: categorical numeric screen
```

## Tune the Strength Tiers

Everything is configurable through
[`framedf_settings()`](https://gillescolling.com/framedf/reference/framedf_settings.md)
or the `...` of
[`frame()`](https://gillescolling.com/framedf/reference/frame.md):

``` r

frame(df,
      strong_threshold   = 0.6,
      moderate_threshold = 0.4,
      weak_threshold     = 0.15)
```

## Next Steps

- [Workflows](https://gillescolling.com/framedf/articles/workflows.md):
  practical examples (large data, repeated measures, observer effects).
- [Roles and
  rules](https://gillescolling.com/framedf/articles/roles.md): what each
  role means and why some columns are excluded from screening.
