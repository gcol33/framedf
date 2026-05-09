# Print a `frame_df` Object

Prints a calm, qualitative narrative of the data frame. The output is
organised into four sections: Structure, Relationships, Anomalies, and
Ignored. No raw correlation values, p-values, or test statistics are
shown – those live in
[`relationships()`](https://gillescolling.com/framedf/reference/relationships.md)
and
[`details()`](https://gillescolling.com/framedf/reference/details.md)
for readers who want them.

## Usage

``` r
# S3 method for class 'frame_df'
print(x, ...)
```

## Arguments

- x:

  A `frame_df` object.

- ...:

  Unused.

## Value

The input invisibly.
