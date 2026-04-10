# Generate the English summary text

Generate the English summary text

## Usage

``` r
generate_english_summary(
  station_summary,
  taxa_lookup = NULL,
  cruise_info = "",
  phyto_groups = NULL,
  provider = NULL,
  unclassified_fractions = NULL
)
```

## Arguments

- station_summary:

  Full station_summary data frame.

- taxa_lookup:

  Optional taxa lookup table.

- cruise_info:

  Cruise info string.

- phyto_groups:

  Optional phytoplankton group table used to provide explicit group
  assignments in the prompt text.

- provider:

  LLM provider (`"openai"` or `"gemini"`). NULL auto-detects.

- unclassified_fractions:

  Optional per-sample unclassified percentages for contextualizing the
  summary.

## Value

Character string with English summary.
