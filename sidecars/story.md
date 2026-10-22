# Story: framedf

## Claim
A first-pass diagnostic for an unfamiliar data frame: column roles, screened pairs of columns and anomalies, reported as sentences.

## Audience
R users receiving a table they did not build (field data, a collaborator's export) who want to know what is in it before modelling.

## Relation
- to: none

## Arcs

### A1 What each column is
- question: what kind of table is this?
- answer: each column gets a role; roles give the structure (observation unit, axes, repeated measures, nesting)
- home: vignettes/roles.Rmd
- also: README

### A2 Which columns move together
- question: what relates to what, and what might be an artefact?
- answer: role-aware pair screening on C++ primitives, grouped as meaningful, suspicious, structural, ignored; optional confounder adjustment
- home: vignettes/workflows.Rmd
- also: README, quickstart

### A3 What looks wrong
- question: which values should I check?
- answer: anomalies, missingness and inflation patterns
- home: vignettes/quickstart.Rmd
- also: README

### A4 Large tables
- question: does it work on millions of rows?
- answer: two-stage progressive subsampling above `subsample_threshold`
- evidence: vignettes/workflows.Rmd (Workflow 4)
- home: vignettes/workflows.Rmd
- also: README

## Non-goals
Not stated in the docs yet.

## Documents
- DESCRIPTION: claim, roles, pair screening, anomalies, scale
- README / landing: claim, list of arcs, installation (compiled), first call on `airquality`, routing
- Get started (quickstart.Rmd): one simulated data frame through `frame()` and the reader functions
- articles: workflows (how-to), roles (explanation)

## Open
- No timing benchmark in the repo backs "millions of rows"; the README states the mechanism only.
