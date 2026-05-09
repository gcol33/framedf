# framedf

[![R-CMD-check](https://github.com/gcol33/framedf/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gcol33/framedf/actions/workflows/R-CMD-check.yml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**First-pass triage for unfamiliar data frames.**

The `framedf` package gives you a one-line read on any data frame: it
infers what each column means, screens every sensible pair for
relationships, and flags the values worth checking before you model. The
output is qualitative (direction, strength, stability), so you can skim
it the way you would skim a colleague’s note.

## Quick Start

``` r

library(framedf)
print(frame(my_data))
```

A typical run on a mid-size ecological table looks like:

    framedf

    5,000 rows × 19 columns

    Structure
    ────────────────
    Looks like a spatial repeated-measure observational dataframe.

    Detected observation unit:
    • plot observation

    Detected temporal structure:
    • sampling_year

    Detected spatial structure:
    • longitude
    • latitude

    Likely identifiers:
    • PlotObservationID
    • PlotID
    • DatasetID
    • observer_id

    Possible grouping structure:
    • repeated observations within PlotID
    • observations grouped by country

    Possible compositional structure:
    • native_cover
    • alien_cover

    Relationships
    ────────────────
    temperature strongly decreases with elevation

    species_richness strongly increases with plot_area

    biomass strongly increases with species_richness

    longitude changes systematically with sampling_year
      possible spatial sampling drift

    the relationship between road_density and neophyte_richness weakens after accounting for country
      possible regional confounding

    DatasetID appears to structure both species_richness and biomass
      possible dataset effect

    observer_id appears to structure both species_richness and biomass
      possible observer effect

    native_cover and alien_cover behave as constrained complements

    Anomalies
    ────────────────
    species_richness, biomass, and zero_heavy_var contain extreme values relative to most observations
    zero_heavy_var shows a strongly skewed distribution

    Missingness
    ────────────────
    soil_pH is missing systematically in older observations

    Inflation and sparsity
    ────────────────
    zero_heavy_var is strongly zero-inflated

    Ignored relationships
    ────────────────
    PlotObservationID was ignored because it behaves like an identifier
    sampling_year was ignored because it is a temporal column, screened separately as drift

## Statement of Need

Every analysis starts with the same task: figure out what each column
is, which pairs covary, and which values look wrong. `framedf` covers
that first pass in one call, and returns findings in language you can
read out loud.

It is useful for:

- exploratory analysis on unfamiliar tabular data,
- pre-modelling sanity checks,
- spotting observer effects, drift, and compositional structure,
- documenting what you saw before you started fitting.

## Features

### Reader functions

- **[`frame()`](https://gillescolling.com/framedf/reference/frame.md)**:
  build a triage object from a data frame.
- **`print(frame(df))`**: narrative overview with **Structure**,
  **Relationships**, **Anomalies**, **Missingness**, **Inflation and
  sparsity**, and **Ignored** sections.
- **[`relationships()`](https://gillescolling.com/framedf/reference/relationships.md)**:
  meaningful, suspicious, structural, and ignored pairs with direction,
  strength, and stability.
- **[`anomalies()`](https://gillescolling.com/framedf/reference/anomalies.md)**:
  per-column oddities grouped by qualitative pattern.
- **[`details()`](https://gillescolling.com/framedf/reference/details.md)**:
  analysis mode, column roles, skipped rules, and which backend ran.
- **[`framedf_settings()`](https://gillescolling.com/framedf/reference/framedf_settings.md)**:
  every threshold is tunable.

### What gets detected

- **Roles**: identifier, administrative index, grouping identifier,
  temporal, latitude, longitude, continuous measurement, compositional
  (cover, share, percent), categorical, logical flag, sparse binary,
  near-constant, constant.
- **Structure**: observation unit, temporal and spatial axes,
  identifiers, grouping membership, repeated measures, nested structure
  (A in B), compositional groups.
- **Relationships**: linear association (with optional confounder
  adjustment), one-way ANOVA-style group effects, temporal and spatial
  drift, constrained-complement compositional pairs, confounded pairs (X
  and Y weaken after adjusting for Z), multi-target group effects (X
  structures both Y and Z), nonlinear pairs, categorical × categorical
  association.
- **Anomalies**: Tukey-fence outliers, moment-based skew, implausible
  coordinate ranges, totals exceeding bounds, inconsistent
  capitalisation, very rare categorical levels, possible lat/lon swap,
  isolated temporal values.
- **Missingness**: systematic over time, clustered by group, jointly
  missing across columns.
- **Inflation and sparsity**: zero-inflation, dominant extreme values,
  discretisation, singleton-heavy categoricals.

### Scale

C++ primitives (Rcpp) handle inner loops. On data frames above
`subsample_threshold` (default 50,000 rows), numeric pair screening uses
two-stage progressive subsampling, which lets `framedf` scale to
millions of rows.

## Installation

``` r

# install.packages("pak")
pak::pak("gcol33/framedf")
```

## Usage Examples

### Adjust for a confounder

``` r

fd <- frame(df, adjustment = "elevation")
relationships(fd)
```

With `adjustment` set, every numeric pair is screened on residualised
values (QR-based partial-out), so confounded pairs no longer surface as
meaningful.

### Tune the strength thresholds

``` r

fd <- frame(df,
            strong_threshold   = 0.6,
            moderate_threshold = 0.4,
            weak_threshold     = 0.15)
```

### Inspect everything

``` r

fd <- frame(df)
print(fd)          # narrative overview
relationships(fd)  # ordered by kind
anomalies(fd)      # ordered by pattern
details(fd)        # how the analysis was done
```

## Documentation

- [Get
  Started](https://gillescolling.com/framedf/articles/quickstart.html)
- [Workflows](https://gillescolling.com/framedf/articles/workflows.html)
- [Roles and
  Rules](https://gillescolling.com/framedf/articles/roles.html)
- [Full Reference](https://gillescolling.com/framedf/reference/)

## Support

> “Software is like sex: it’s better when it’s free.”
>
> Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE.md file)

## Citation

``` bibtex
@software{framedf,
  author = {Colling, Gilles},
  title  = {framedf: First-Pass Triage of Unfamiliar Data Frames},
  year   = {2026},
  url    = {https://github.com/gcol33/framedf}
}
```
