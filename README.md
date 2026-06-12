# framedf

> Small exact engines for scientific computing in R.

*a first look at someone else's table*

[![R-CMD-check](https://github.com/gcol33/framedf/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gcol33/framedf/actions/workflows/R-CMD-check.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**First-pass triage of an unfamiliar data frame: column roles, pairwise relationships, and anomalies, read out in sentences.**

Hand it a table. `framedf` infers what each column is (identifier, temporal,
spatial, compositional, measurement), screens every sensible pair of columns for
relationships, and flags the values worth checking before you model. Pair
screening runs on C++ primitives (Rcpp): ordinary least squares for numeric
pairs, QR residualisation when you adjust for a confounder, one-way ANOVA with
eta-squared for group effects. `str()` and `skimr` describe each column on its
own; `framedf` reads the columns against each other and tells you what it found.

```r
library(framedf)

print(frame(my_data))
```

## What a run reads like

```
framedf

5,000 rows x 19 columns

Structure
────────────────
Looks like a spatial repeated-measure observational dataframe.

Detected temporal structure:
• sampling_year

Detected spatial structure:
• longitude
• latitude

Relationships
────────────────
temperature strongly decreases with elevation

species_richness strongly increases with plot_area

longitude changes systematically with sampling_year
  possible spatial sampling drift

the relationship between road_density and neophyte_richness weakens after accounting for country
  possible regional confounding

observer_id appears to structure both species_richness and biomass
  possible observer effect

native_cover and alien_cover behave as constrained complements

Anomalies
────────────────
zero_heavy_var shows a strongly skewed distribution

Missingness
────────────────
soil_pH is missing systematically in older observations
```

The output is qualitative (direction, strength, stability), so you can skim it
the way you would skim a colleague's note on the data.

## What it detects

- **Roles**: identifier, administrative index, grouping identifier, temporal,
  latitude, longitude, continuous measurement, compositional (cover, share,
  percent), categorical, logical flag, sparse binary, near-constant, constant.
- **Structure**: observation unit, temporal and spatial axes, repeated measures,
  nested structure (A within B), compositional groups.
- **Relationships**: linear association (with optional confounder adjustment),
  one-way group effects, temporal and spatial drift, constrained-complement
  compositional pairs, confounded pairs (X and Y weaken after adjusting for Z),
  multi-target group effects (X structures both Y and Z), categorical pairs.
- **Anomalies**: Tukey-fence outliers, moment-based skew, implausible coordinate
  ranges, totals exceeding bounds, inconsistent capitalisation, rare categorical
  levels, possible latitude/longitude swap.
- **Missingness, inflation, sparsity**: missing-over-time, missing-by-group,
  zero-inflation, dominant extreme values, singleton-heavy categoricals.

## Reader functions

- **`frame()`**: build a triage object from a data frame.
- **`relationships()`**: meaningful, suspicious, structural, and ignored pairs,
  with direction, strength, and stability.
- **`anomalies()`**: per-column oddities, grouped by qualitative pattern.
- **`details()`**: screening mode, the role assigned to each column, the rules
  that skipped some pairs, and which backend ran.
- **`framedf_settings()`**: every threshold is tunable.

## Adjust for a confounder

```r
fd <- frame(df, adjustment = "elevation")
relationships(fd)
```

With `adjustment` set, every numeric pair is screened on residualised values
(QR-based partial-out), so a pair driven by the confounder no longer surfaces as
meaningful.

## Tune the thresholds

```r
fd <- frame(df,
            strong_threshold   = 0.6,
            moderate_threshold = 0.4,
            weak_threshold     = 0.15)
```

## Scale

Above `subsample_threshold` (default 50,000 rows), numeric pair screening uses
two-stage progressive subsampling: low-association pairs are dropped after a
small probe sample, and the survivors are re-screened on a larger confirmation
sample. The C++ primitives carry the inner loops, so screening stays usable into
the millions of rows.

## Installation

```r
install.packages("pak")
pak::pak("gcol33/framedf")
```

## Documentation

- [Get Started](https://gillescolling.com/framedf/articles/quickstart.html)
- [Workflows](https://gillescolling.com/framedf/articles/workflows.html)
- [Roles and Rules](https://gillescolling.com/framedf/articles/roles.html)
- [Reference](https://gillescolling.com/framedf/reference/)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good
tools should be free and open. I started these projects for my own work and
figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say
thanks. It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE.md file)

## Citation

```bibtex
@software{framedf,
  author = {Colling, Gilles},
  title  = {framedf: First-Pass Triage of Unfamiliar Data Frames},
  year   = {2026},
  url    = {https://github.com/gcol33/framedf}
}
```
