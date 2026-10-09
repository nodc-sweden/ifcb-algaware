# Read the top-scoring class per ROI, before thresholding, from an H5 file

Uses `class_name_auto` when present. Older files without it fall back to
the highest-scoring entry of `class_labels`; files without either give
`NA`.

## Usage

``` r
read_auto_classes(h5, output_scores)
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md).

- output_scores:

  Score matrix (classes x ROIs) already read from `h5`.

## Value

Character vector with one class per ROI.
