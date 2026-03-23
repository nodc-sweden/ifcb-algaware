# Load AlgAware station definitions

Loads the bundled station list and appends any extra stations from
settings.

## Usage

``` r
load_algaware_stations(extra_stations = list())
```

## Arguments

- extra_stations:

  A list of extra station definitions, each with `STATION_NAME`,
  `COAST`, and `STATION_NAME_SHORT`.

## Value

A data.frame with STATION_NAME, COAST, STATION_NAME_SHORT columns.
