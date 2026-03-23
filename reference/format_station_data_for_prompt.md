# Format station data as a text summary for the LLM prompt

Format station data as a text summary for the LLM prompt

## Usage

``` r
format_station_data_for_prompt(station_data, taxa_lookup = NULL)
```

## Arguments

- station_data:

  Data frame with station_summary rows for one visit.

- taxa_lookup:

  Optional taxa lookup with HAB column.

## Value

Character string describing the station data.
