# Generate the Swedish summary text

Generate the Swedish summary text

## Usage

``` r
generate_swedish_summary(
  station_summary,
  taxa_lookup = NULL,
  cruise_info = "",
  provider = NULL
)
```

## Arguments

- station_summary:

  Full station_summary data frame.

- taxa_lookup:

  Optional taxa lookup table.

- cruise_info:

  Cruise info string.

- provider:

  LLM provider (`"openai"` or `"gemini"`). NULL auto-detects.

## Value

Character string with Swedish summary.
