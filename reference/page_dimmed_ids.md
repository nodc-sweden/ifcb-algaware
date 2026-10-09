# Dimmed image IDs among the images of the current gallery page

Dimmed image IDs among the images of the current gallery page

## Usage

``` r
page_dimmed_ids(imgs, dimmed)
```

## Arguments

- imgs:

  Data.frame of the page's images (`sample_name`, `roi_number`), or
  `NULL` when no page is shown.

- dimmed:

  Character vector of image IDs to dim (`"<sample_name>_<roi_number>"`).

## Value

The IDs of the page's images that are in `dimmed`.
