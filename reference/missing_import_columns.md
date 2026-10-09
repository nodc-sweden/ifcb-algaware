# Columns an imported corrections file lacks

The correction columns are always required. A file with threshold rows
must also have the columns
[`adjustments_from_import()`](https://nodc-sweden.github.io/ifcb-algaware/reference/adjustments_from_import.md)
reads, which a hand-edited or truncated file may have lost.

## Usage

``` r
missing_import_columns(df)
```

## Arguments

- df:

  Data.frame read from a corrections CSV.

## Value

Character vector of the missing column names, empty when the file can be
imported.
