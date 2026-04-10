# Build a station-level group summary block for LLM prompts

Build a station-level group summary block for LLM prompts

## Usage

``` r
build_station_group_summary(station_data, group_col, heading, n_top = NULL)
```

## Arguments

- station_data:

  Station summary rows for one visit, including `detailed_group` or
  `text_group`.

- group_col:

  Column name to summarize.

- heading:

  Heading line for the block.

## Value

Character string.
