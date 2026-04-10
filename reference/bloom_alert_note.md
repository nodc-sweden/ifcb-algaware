# Detect bloom conditions and return language-specific prompt instructions

Checks whether cruise data meets the criteria for a spring bloom (West
Coast, Jan-Feb: diatom-dominated, chl \> 3 ug/L) or a cyanobacterial
bloom (Baltic, Jun-Aug: cyanobacteria-dominated, chl \> 3 ug/L) and
returns a block of extra instructions to append to the LLM prompt when
either condition is met.

## Usage

``` r
bloom_alert_note(station_summary, phyto_groups = NULL, lang = "en")
```

## Arguments

- station_summary:

  Full station_summary data frame.

- phyto_groups:

  Optional phytoplankton group table.

- lang:

  `"en"` (English) or `"sv"` (Swedish).

## Value

Character string with bloom alert instructions, or `""`.
