# Generate the Swedish summary text

Generate the Swedish summary text

## Usage

``` r
generate_swedish_summary(
  station_summary,
  taxa_lookup = NULL,
  cruise_info = "",
  phyto_groups = NULL,
  provider = NULL,
  unclassified_fractions = NULL,
  chl_measure = "fluorescence",
  microscopy = NULL
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

  LLM provider (`"openai"`, `"gemini"` or `"claude"`). NULL
  auto-detects.

- unclassified_fractions:

  Optional per-sample fractions of unclassified detections supplied to
  the prompt.

- chl_measure:

  How the active chlorophyll source is measured, `"fluorescence"`
  (FerryBox/CTD) or `"concentration"` (LIMS bottle/hose filter samples).
  Adjusts the chlorophyll terminology used.

- microscopy:

  Optional enriched microscopy observations for stations without IFCB
  data (see
  [`enrich_microscopy_taxa()`](https://nodc-sweden.github.io/ifcb-algaware/reference/enrich_microscopy_taxa.md)).

## Value

Character string with Swedish summary.
