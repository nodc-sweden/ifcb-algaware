# Create a stacked bar chart of relative biovolume

HAB species are marked with a red asterisk (\*) in the legend.

## Usage

``` r
create_stacked_bar(wide_summary, taxa_lookup = NULL, n_top = 10, title = "")
```

## Arguments

- wide_summary:

  Wide-format data from
  [`create_wide_summary()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/create_wide_summary.md).

- taxa_lookup:

  Optional taxa lookup table with `HAB` column.

- n_top:

  Number of top taxa to show individually. Default 10.

- title:

  Plot title.

## Value

A ggplot object.
