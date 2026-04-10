# Format cruise-level data summary for LLM prompt

Format cruise-level data summary for LLM prompt

## Usage

``` r
format_cruise_summary_for_prompt(
  station_summary,
  taxa_lookup = NULL,
  unclassified_fractions = NULL,
  phyto_groups = NULL
)
```

## Arguments

- station_summary:

  Full station_summary data frame.

- taxa_lookup:

  Optional taxa lookup with `HAB` and `warning_level` columns.

- phyto_groups:

  Optional phytoplankton group table used to provide explicit group
  assignments in the prompt text.

## Value

Character string with cruise-level overview.
