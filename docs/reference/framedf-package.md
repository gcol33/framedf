# framedf: Calm Triage of Unfamiliar Data Frames

`framedf` looks at a data frame the way an experienced analyst does in
the first thirty seconds: it infers what each column means, screens
every sensible pair for relationships, and lists the anomalies worth
reading first. The output is qualitative — direction, strength,
stability — not raw test statistics.

## Entry points

- [`frame()`](https://gillescolling.com/framedf/reference/frame.md):

  build a triage object from a data frame.

- [`print.frame_df()`](https://gillescolling.com/framedf/reference/print.frame_df.md):

  narrative overview.

- [`relationships()`](https://gillescolling.com/framedf/reference/relationships.md):

  meaningful, suspicious, structural, ignored pairs.

- [`anomalies()`](https://gillescolling.com/framedf/reference/anomalies.md):

  per-column oddities.

- [`details()`](https://gillescolling.com/framedf/reference/details.md):

  analysis mode, roles, skipped rules, backend.

- [`framedf_settings()`](https://gillescolling.com/framedf/reference/framedf_settings.md):

  tunable thresholds.

## See also

Useful links:

- <https://gillescolling.com/framedf/>

- <https://github.com/gcol33/framedf>

- Report bugs at <https://github.com/gcol33/framedf/issues>

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
([ORCID](https://orcid.org/0000-0003-3070-6066)) \[copyright holder\]
