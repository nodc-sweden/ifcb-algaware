# Add front page to the report

Inserts a professional cover page with the AlgAware logo, report title,
issue number, diary number placeholder, cruise information, and cruise
phytoplankton group-composition map. Mosaics are placed on the following
page via
[`add_mosaic_overview()`](https://nodc-sweden.github.io/ifcb-algaware/reference/add_mosaic_overview.md).

## Usage

``` r
add_front_page(
  doc,
  cleanup,
  taxa_lookup = NULL,
  cruise_info = "",
  report_number = NULL,
  report_dnr = NULL,
  image_counts = NULL,
  group_map_plot = NULL
)
```

## Arguments

- doc:

  An rdocx object.

- cleanup:

  Environment with a `files` character vector.

- taxa_lookup:

  Optional taxa lookup table with `italic` column.

- cruise_info:

  Cruise information string (e.g. "RV Svea March cruise, 2026-03-15 to
  2026-03-22").

- report_number:

  Optional report issue number (e.g. "1").

- report_dnr:

  Optional diarienummer string (e.g. "2026-1234").

- image_counts:

  Optional image counts data for the cruise track map.

- group_map_plot:

  Optional phytoplankton group composition pie map for the front page.

## Value

The modified rdocx object.
