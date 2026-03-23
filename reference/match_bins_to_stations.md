# Match dashboard metadata to AlgAware stations using spatial join

Finds IFCB bins sampled near AlgAware stations by creating buffers
around station centroids and performing a spatial join.

## Usage

``` r
match_bins_to_stations(metadata, algaware_stations)
```

## Arguments

- metadata:

  A data.frame with at least `latitude` and `longitude` columns (from
  dashboard metadata).

- algaware_stations:

  A data.frame from
  [`load_algaware_stations()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/load_algaware_stations.md).

## Value

A tibble of metadata rows matched to stations, with `STATION_NAME`,
`COAST`, and `STATION_NAME_SHORT` columns added.
