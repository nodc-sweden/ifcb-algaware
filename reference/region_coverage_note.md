# Restrict the summary to the regions that were actually sampled

The writing guide asks for one paragraph per region. When a cruise only
has stations on one coast, this returns an instruction telling the LLM
to write only that region's paragraph, so it does not invent a
description of the coast that has no data. Microscopy stations count as
sampling their coast; a coast covered by microscopy only gets an
instruction to keep its paragraph qualitative.

## Usage

``` r
region_coverage_note(station_summary, microscopy = NULL)
```

## Arguments

- station_summary:

  Full station_summary data frame.

- microscopy:

  Optional microscopy observations (with `COAST`) for stations without
  IFCB data.

## Value

Character string with the instruction, or `""` when both coasts have
IFCB data (or no coast has any data).
