# Create a heatmap of biovolume by species and station

HAB species are marked with a red asterisk (\*) on the y-axis labels.

## Usage

``` r
create_heatmap(
  wide_summary,
  taxa_lookup = NULL,
  title = "",
  sample_counts = NULL
)
```

## Arguments

- wide_summary:

  Wide-format data from
  [`create_wide_summary()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/create_wide_summary.md).

- taxa_lookup:

  Optional taxa lookup table with `HAB` column. If provided, HAB species
  are annotated with a red asterisk on the y-axis.

- title:

  Plot title.

- sample_counts:

  Optional named integer vector mapping station_date column names to
  number of samples. If provided, `n = X` is appended to each x-axis
  label.

## Value

A ggplot object.
