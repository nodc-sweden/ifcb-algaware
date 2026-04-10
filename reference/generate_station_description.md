# Generate a station description

Generate a station description

## Usage

``` r
generate_station_description(
  station_data,
  taxa_lookup = NULL,
  all_stations_summary = NULL,
  phyto_groups = NULL,
  provider = NULL,
  unclassified_pct = NULL
)
```

## Arguments

- station_data:

  Data frame with station_summary rows for one visit.

- taxa_lookup:

  Optional taxa lookup table.

- all_stations_summary:

  Optional full station_summary for context.

- phyto_groups:

  Optional phytoplankton group table used to provide explicit group
  assignments in the prompt text.

- provider:

  LLM provider (`"openai"` or `"gemini"`). NULL auto-detects.

- unclassified_pct:

  Optional per-class unclassified percentage info used for context.

## Value

Character string with station description in English.
