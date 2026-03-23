# Save a ggplot to a temp file and add to document

Save a ggplot to a temp file and add to document

## Usage

``` r
add_plot_to_doc(
  doc,
  plot,
  cleanup,
  width,
  height,
  display_width,
  display_height
)
```

## Arguments

- doc:

  An rdocx object.

- plot:

  A ggplot object.

- cleanup:

  Environment with a `files` character vector for tracking temp files.

- width, height:

  Plot dimensions in inches for ggsave.

- display_width, display_height:

  Display dimensions in the document.

## Value

The modified rdocx object.
