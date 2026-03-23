# Create biomass and chlorophyll maps

Create biomass and chlorophyll maps

## Usage

``` r
create_biomass_maps(station_summary)
```

## Arguments

- station_summary:

  Aggregated station data from
  [`aggregate_station_data()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/aggregate_station_data.md).

## Value

A list with `biomass_map` and `chl_map` ggplot objects.
