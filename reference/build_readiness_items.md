# Build the report readiness item list

Build the report readiness item list

## Usage

``` r
build_readiness_items(
  data_loaded,
  ctd_loaded,
  baltic_mosaic,
  westcoast_mosaic,
  microscopy_stations = character(0)
)
```

## Arguments

- data_loaded:

  Logical, whether IFCB data has been loaded.

- ctd_loaded:

  Logical, whether CTD data has been loaded.

- baltic_mosaic:

  Front-page Baltic mosaic or `NULL`.

- westcoast_mosaic:

  Front-page West Coast mosaic or `NULL`.

- microscopy_stations:

  Names of the stations filled in with microscopy samples; listed as an
  extra item when there are any.

## Value

List of items with `ok` (logical) and `label` (character).
