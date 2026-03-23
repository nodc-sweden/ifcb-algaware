# Create wide-format summary for a region

Create wide-format summary for a region

## Usage

``` r
create_wide_summary(station_summary, coast)
```

## Arguments

- station_summary:

  Aggregated station data from
  [`aggregate_station_data()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/aggregate_station_data.md).

- coast:

  "EAST" for Baltic Sea or "WEST" for West Coast.

## Value

A wide-format data.frame with scientific names as rows and station-date
as columns.
