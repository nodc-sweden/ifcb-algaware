# Order gallery images, optionally by ascending classifier score

Order gallery images, optionally by ascending classifier score

## Usage

``` r
order_images(imgs, by_score)
```

## Arguments

- imgs:

  Data.frame of images, possibly with a `score` column.

- by_score:

  If `TRUE`, sort by ascending score so borderline images come first.

## Value

`imgs`, reordered when requested and possible.
