# Create an image concentration map from cruise metadata

Plots per-sample image concentration (images per litre) along the cruise
track, showing spatial distribution of cell abundance as measured by the
IFCB.

## Usage

``` r
create_image_count_map(image_counts, legend_position = "right", title = NULL)
```

## Arguments

- image_counts:

  Data frame from
  [`fetch_image_counts()`](https://nodc-sweden.github.io/ifcb-algaware/reference/fetch_image_counts.md)
  with columns: latitude, longitude, n_images, ml_analyzed.

- legend_position:

  Legend position. Default `"right"`.

- title:

  Optional plot title string.

## Value

A ggplot object.
