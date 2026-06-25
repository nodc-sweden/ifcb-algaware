# Format cruise-level data summary for LLM prompt

Format cruise-level data summary for LLM prompt

## Usage

``` r
format_cruise_summary_for_prompt(
  station_summary,
  taxa_lookup = NULL,
  unclassified_fractions = NULL,
  phyto_groups = NULL,
  chl_measure = "fluorescence"
)
```

## Arguments

- station_summary:

  Full station_summary data frame.

- taxa_lookup:

  Optional taxa lookup with `HAB` and `warning_level` columns.

- unclassified_fractions:

  Optional named list of per-visit unclassified percentages used to
  annotate station lines.

- phyto_groups:

  Optional phytoplankton group table used to provide explicit group
  assignments in the prompt text.

- chl_measure:

  How the chlorophyll value is measured, `"fluorescence"` (FerryBox/CTD)
  or `"concentration"` (LIMS bottle/hose filter samples). Controls how
  the value is labelled.

## Value

Character string with cruise-level overview.
