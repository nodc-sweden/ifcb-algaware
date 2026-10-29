# Match a microscopy sample to an AlgAware station

Tries, in order: the station name (case-insensitive, against both the
full and short AlgAware names), the synonyms in the SHARK station
register, and the sample position within the station's
`OUT_OF_BOUNDS_RADIUS` (the same rule used for IFCB bins).

## Usage

``` r
match_microscopy_station(
  station_reported,
  latitude,
  longitude,
  algaware_stations,
  shark_stations
)
```

## Arguments

- station_reported:

  Station name written in the sample.

- latitude, longitude:

  Sample position in decimal degrees, or NA.

- algaware_stations:

  Data frame from
  [`load_algaware_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_algaware_stations.md).

- shark_stations:

  SHARK station register from
  [`load_shark_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_shark_stations.md).

## Value

A one-row data.frame with `STATION_NAME`, `COAST`, `STATION_NAME_SHORT`
and `match_method` (all NA when no station matches).
