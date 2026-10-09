# Body of the "Import Corrections" confirmation dialog

Body of the "Import Corrections" confirmation dialog

## Usage

``` r
import_preview_ui(
  corrections,
  thresholds,
  file_name,
  n_current,
  n_current_thresholds
)
```

## Arguments

- corrections:

  Correction rows from
  [`split_corrections_import()`](https://nodc-sweden.github.io/ifcb-algaware/reference/split_corrections_import.md).

- thresholds:

  Result of
  [`adjustments_from_import()`](https://nodc-sweden.github.io/ifcb-algaware/reference/adjustments_from_import.md).

- file_name:

  Name of the imported file.

- n_current:

  Number of corrections in the current session.

- n_current_thresholds:

  Number of threshold adjustments in the current session.

## Value

A
[`shiny::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html).
