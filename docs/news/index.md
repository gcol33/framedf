# Changelog

## framedf 0.1.0

Initial public release.

### Triage

- [`frame()`](https://gillescolling.com/framedf/reference/frame.md)
  builds a single triage object from a data frame, infers a semantic
  role for every column, screens all sensible pairs of columns, and
  lists anomalies in one pass.
- Roles: `id`, `admin_index`, `group_id`, `temporal`, `coord_lat`,
  `coord_lon`, `measurement`, `compositional`, `categorical`, `flag`,
  `near_constant`, `constant`, `unknown`.

### Reader functions

- [`print.frame_df()`](https://gillescolling.com/framedf/reference/print.frame_df.md)
  — narrative overview with **Structure**, **Relationships**,
  **Anomalies**, and **Ignored** sections. Output is qualitative; no raw
  test statistics in the default view.
- [`relationships()`](https://gillescolling.com/framedf/reference/relationships.md)
  — relationships grouped into **meaningful**, **suspicious**,
  **structural**, and **ignored** with direction, strength, stability,
  and method per pair.
- [`anomalies()`](https://gillescolling.com/framedf/reference/anomalies.md)
  — per-column oddities grouped by pattern.
- [`details()`](https://gillescolling.com/framedf/reference/details.md)
  — analysis mode, column roles, skipped rules, backend.

### Screening

- Numeric × numeric: simple OLS, optionally residualised against an
  adjustment set via `adjustment = c(...)`.
- Two-stage progressive subsampling for large data: a small probe sample
  drops obviously weak pairs, surviving pairs are confirmed on a larger
  sample. Stability tier (`high` / `medium` / `low`) reflects agreement
  across passes.
- Categorical × numeric: one-way analysis-of-variance summaries with eta
  squared. Strong group effects on identifier-like categoricals are
  flagged as **suspicious** (possible observer effect).
- Temporal × spatial drift: a dedicated check that surfaces sampling
  designs where coordinates move systematically with time.
- Compositional pairs: cover-like columns whose pairwise sum is stable
  are flagged as **structural** (constrained complement).

### Anomalies

- Tukey-fence outliers and moment-based skew on continuous columns.
- Implausible coordinate ranges (latitude / longitude bounds).
- Compositional totals that exceed the inferred 0–1 or 0–100 scale.
- Inconsistent capitalisation in categorical columns.
- Very rare categorical levels (configurable via `min_level_n`).

### Tunables

- [`framedf_settings()`](https://gillescolling.com/framedf/reference/framedf_settings.md)
  exposes every threshold: `min_obs`, `strong_threshold`,
  `moderate_threshold`, `weak_threshold`, `near_constant_ratio`,
  `id_unique_ratio`, `skew_threshold`, `subsample_threshold`,
  `subsample_probe`, `subsample_confirm`, `compositional_cv`,
  `observer_min_levels`, `min_level_n`, `seed`.

### Backend

- C++ primitives (Rcpp) for `lm_simple`, `residualize`, and
  `group_summary`. Falls back to pure-R implementations when the shared
  library is not loaded.
