# Check whether an H5 classification file holds no datasets

ifcb-classify writes such a file for a bin without any images.

## Usage

``` r
h5_is_empty(h5)
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md).

## Value

Logical scalar.
