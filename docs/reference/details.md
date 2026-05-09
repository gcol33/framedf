# Show Methods, Roles, and Skipped Rules

`details()` prints how
[`frame()`](https://gillescolling.com/framedf/reference/frame.md) did
its work: the screening mode it chose, the role assigned to each column,
the rules that caused some pairs to be skipped, and which backend was
used.

## Usage

``` r
details(x, ...)

# S3 method for class 'frame_df'
details(x, ...)
```

## Arguments

- x:

  A `frame_df` object.

- ...:

  Additional arguments (unused).

## Value

The input invisibly.

## Details

This is the place to look when an output from
[`print.frame_df()`](https://gillescolling.com/framedf/reference/print.frame_df.md)
or
[`relationships()`](https://gillescolling.com/framedf/reference/relationships.md)
surprises you and you want to know why a particular pair was or was not
screened.

## Examples

``` r
df <- data.frame(x = 1:30, y = rnorm(30))
details(frame(df))
```
