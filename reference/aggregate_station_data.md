# Aggregate biovolume data per station visit

Aggregates sample volumes, computes per-liter concentrations, and
assigns presence categories.

## Usage

``` r
aggregate_station_data(biovolume_data, metadata)
```

## Arguments

- biovolume_data:

  Output from
  [`summarize_biovolumes()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/summarize_biovolumes.md).

- metadata:

  Station-matched metadata with `STATION_NAME`, `COAST`,
  `STATION_NAME_SHORT`, `sample_time`, and `ml_analyzed` columns.

## Value

A data.frame with per-station, per-taxon summary including
`counts_per_liter`, `biovolume_mm3_per_liter`, `carbon_ug_per_liter`,
and `Presence_cat`.
