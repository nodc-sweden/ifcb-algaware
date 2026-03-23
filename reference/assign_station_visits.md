# Group bins into station visits

Handles cases where a station is visited multiple times during a cruise
(days apart), and where bins at the same station span midnight.

## Usage

``` r
assign_station_visits(metadata, max_gap_hours = 12)
```

## Arguments

- metadata:

  A data.frame with `STATION_NAME` and `sample_time` columns (POSIXct).

- max_gap_hours:

  Maximum gap between consecutive bins to consider them part of the same
  visit. Default 12 hours. The ship may pass the same station twice on
  different days; this threshold splits those into separate visits.

## Value

The input data.frame with an added `visit_id` column (character:
`STATION_NAME_visitN`).
