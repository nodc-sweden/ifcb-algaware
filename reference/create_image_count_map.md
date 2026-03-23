# Create an image count map from cruise metadata

Plots per-sample image counts along the cruise track, showing spatial
distribution of cell abundance as measured by the IFCB.

## Usage

``` r
create_image_count_map(image_counts)
```

## Arguments

- image_counts:

  Data frame from
  [`fetch_image_counts()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/fetch_image_counts.md)
  with columns: latitude, longitude, n_images.

## Value

A ggplot object.
