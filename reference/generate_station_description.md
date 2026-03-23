# Generate a station description

Generate a station description

## Usage

``` r
generate_station_description(
  station_data,
  taxa_lookup = NULL,
  all_stations_summary = NULL,
  provider = NULL
)
```

## Arguments

- station_data:

  Data frame with station_summary rows for one visit.

- taxa_lookup:

  Optional taxa lookup table.

- all_stations_summary:

  Optional full station_summary for context.

- provider:

  LLM provider (`"openai"` or `"gemini"`). NULL auto-detects.

## Value

Character string with station description in English.
