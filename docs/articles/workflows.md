# Workflows

This vignette walks through three realistic situations: an observer
effect hidden in a survey, a sampling design that has drifted in space
over time, and a compositional pair that should not be modelled
independently. In every case the goal is the same: read the data once
before doing any modelling.

## Workflow 1: An Observer Effect

**Goal.** A dataset of plant richness measurements has multiple
observers. We want to know whether observer identity influences the
recorded richness more than the ecology does.

**Challenge.** Standard correlation matrices ignore categorical
variables, so the observer dimension is invisible to most diagnostic
tools.

**Strategy.** `framedf` runs a one-way ANOVA between every categorical
column and every measurement column. If the categorical column is
identifier-like and the group effect is large, the pair is reclassified
as **suspicious** with the concern *“possible observer effect”*.

``` r

set.seed(1)
n <- 400L
obs   <- sample(letters[1:8], n, replace = TRUE)
bias  <- stats::setNames(stats::rnorm(8, 0, 4), letters[1:8])
df <- data.frame(
  observer_id = obs,
  plot_area   = stats::runif(n, 1, 100),
  richness    = NA_real_,
  stringsAsFactors = FALSE
)
df$richness <- 10 + 0.3 * sqrt(df$plot_area) + bias[df$observer_id] +
               stats::rnorm(n)
```

``` r

fd <- frame(df)
relationships(fd, kind = "suspicious")
#> Relationships
#> 
#> suspicious
#> ────────────────
#> richness ~ observer_id
#>   pattern: group effect
#>   strength: strong
#>   stability: high
#>   method: categorical numeric screen
#>   concern: possible observer effect
```

The screen flags `richness ~ observer_id` as a group effect with a
possible observer-style concern. Note that the meaningful
`richness ~ plot_area` relationship is still present in the
`"meaningful"` block; the suspicious flag does not erase it.

``` r

relationships(fd, kind = "meaningful")
#> Relationships
#> No relationships meet the criteria.
```

## Workflow 2: Spatial Sampling Drift

**Goal.** A long-running monitoring program has a year column and
latitude / longitude columns. We want to know whether the geographic
sampling has shifted over time.

**Challenge.** Time and space appear in many tables but are rarely
checked together, yet a slow drift is a common source of confounded
trend estimates.

**Strategy.** `framedf` runs a dedicated drift screen for every temporal
× spatial pair. If the slope is moderate or strong, the pair is reported
as suspicious with the concern *“sampling design may be spatially
structured”*.

``` r

set.seed(2)
yr <- sample(2000:2020, 500, replace = TRUE)
df_drift <- data.frame(
  year      = yr,
  latitude  = stats::runif(500, 40, 50),
  longitude = 10 + (yr - 2010) * 0.3 + stats::rnorm(500, sd = 0.1),
  richness  = stats::rnorm(500, 30)
)
fd_drift <- frame(df_drift)
relationships(fd_drift, kind = "suspicious")
#> Relationships
#> 
#> suspicious
#> ────────────────
#> longitude ~ year
#>   pattern: temporal spatial drift
#>   strength: strong
#>   stability: high
#>   method: drift screen
#>   concern: possible spatial sampling drift
```

The drift between `year` and `longitude` is surfaced as suspicious.
Plain numeric pair screening would have hidden this, since `year` and
`longitude` are both excluded from the symmetric numeric sweep because
they are role-tagged as temporal and spatial respectively.

## Workflow 3: A Constrained Complement

**Goal.** Two cover variables (native cover and alien cover) are
recorded as fractions on the same plot. We want the package to treat
them as a constrained pair, since they always sum to one.

**Challenge.** A naive correlation between the two is mechanically
strong and would hijack the *meaningful* section.

**Strategy.** Compositional pairs whose pairwise sum is approximately
constant are routed to a separate **structural** block. They are still
visible (the user can decide what to do) but they are not mixed in with
the genuine ecological signals.

``` r

set.seed(3)
n <- 200L
df_comp <- data.frame(
  native_cover = stats::runif(n, 0, 1),
  richness     = stats::rnorm(n, 30)
)
df_comp$alien_cover  <- 1 - df_comp$native_cover
df_comp$temperature  <- 20 - 0.01 * df_comp$richness + stats::rnorm(n)
fd_comp <- frame(df_comp)
relationships(fd_comp)
#> Relationships
#> 
#> meaningful
#> ────────────────
#> alien_cover ~ native_cover
#>   direction: negative
#>   strength: strong
#>   stability: high
#>   method: numeric screening
#> 
#> structural
#> ────────────────
#> alien_cover ~ native_cover
#>   pattern: constrained complement
#>   concern: compositional relationship
#>   method: compositional sum check
```

The `native_cover ~ alien_cover` pair shows up under **structural**,
described as a constrained complement, while the genuine
`richness ~ temperature` association lives under **meaningful**.

## Workflow 4: Large Data with Subsampling

**Goal.** A million-row table is too big to screen fully on every pair.
We want sensible relationships in seconds.

**Challenge.** Naive pair screening on millions of rows multiplies
through too many regressions.

**Strategy.** Above `subsample_threshold` rows (default 50 000),
`framedf` runs a small probe sample first and drops obviously weak
pairs. Surviving pairs are confirmed on a larger sample. Stability is
recorded per pair: `high` if both passes agree on tier, `medium` if they
disagree, `low` if the probe alone was carried.

``` r

fd_big <- frame(big_df)
details(fd_big)
```

[`details()`](https://gillescolling.com/framedf/reference/details.md)
will report *“relationship screening used progressive subsampling”*
whenever the input was above the threshold.

## Summary

| Pattern | Where it appears in the output | Concern raised |
|----|----|----|
| measurement × measurement | meaningful | direction + strength |
| many-level group × measurement | suspicious | possible observer effect |
| temporal × spatial | suspicious | sampling design may be spatially structured |
| compositional × compositional | structural | compositional relationship |
| identifier × anything | ignored | excluded from screening |
| near-constant × anything | ignored | excluded from screening |
