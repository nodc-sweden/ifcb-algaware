# Format station data as a text summary for the LLM prompt

Format station data as a text summary for the LLM prompt

## Usage

``` r
format_station_data_for_prompt(
  station_data,
  taxa_lookup = NULL,
  unclassified_pct = NULL,
  phyto_groups = NULL,
  chl_measure = "fluorescence"
)
```

## Arguments

- station_data:

  Data frame with station_summary rows for one visit.

- taxa_lookup:

  Optional taxa lookup with `HAB` and `warning_level` columns.

- unclassified_pct:

  Optional numeric; percentage of unclassified detections at this
  station, used for context notes.

- phyto_groups:

  Optional phytoplankton group table used to provide explicit group
  assignments in the prompt text.

- chl_measure:

  How the chlorophyll value is measured, `"fluorescence"` (FerryBox/CTD)
  or `"concentration"` (LIMS bottle/hose filter samples). Controls how
  the value is labelled.

## Value

Character string describing the station data.
