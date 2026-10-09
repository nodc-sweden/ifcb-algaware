# Reset per-cruise validation state

Clears the corrections log, user-added custom classes, class threshold
adjustments and the gallery selection when new data is loaded, so
corrections made on one cruise are never carried into – and exported or
auto-saved together with – the next cruise loaded in the same session.
Column structure is preserved.

## Usage

``` r
reset_corrections_state(rv)
```

## Arguments

- rv:

  [`shiny::reactiveValues`](https://rdrr.io/pkg/shiny/man/reactiveValues.html)
  (or a list-like object) holding `corrections`, `custom_classes`,
  `selected_images`, `threshold_adjustments`, `threshold_dimmed` and
  `load_count`.

## Value

`rv`, invisibly, after modification.

## Details

Also counts the load in `rv$load_count`. Per-load state kept elsewhere
(the state last written by the auto-save, the gallery page) keys on that
counter rather than on the loaded data changing, because loading the
same cruise again assigns identical data and so invalidates nothing.
