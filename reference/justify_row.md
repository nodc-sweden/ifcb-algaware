# Justify a row of images to a target width

Justify a row of images to a target width

## Usage

``` r
justify_row(imgs_row, widths_row, target_row_width, target_height, bg_color)
```

## Arguments

- imgs_row:

  List of magick image objects.

- widths_row:

  Numeric vector of image widths.

- target_row_width:

  Target total row width in pixels.

- target_height:

  Row height in pixels.

- bg_color:

  Background fill color (hex string).

## Value

A single magick image for the row.
