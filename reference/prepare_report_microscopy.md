# Select and enrich the microscopy samples for a report

Select and enrich the microscopy samples for a report

## Usage

``` r
prepare_report_microscopy(loaded, station_summary, taxa_lookup)
```

## Arguments

- loaded:

  Result of
  [`load_microscopy_samples()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_microscopy_samples.md),
  or NULL.

- station_summary:

  IFCB station summary used for the report.

- taxa_lookup:

  Taxa lookup used for the report.

## Value

A list with `obs` (enriched observations, or NULL when no sample fills a
gap) and `notes` (messages for the user).
