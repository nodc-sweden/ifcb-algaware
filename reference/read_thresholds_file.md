# Read the trained thresholds of a single H5 file

Read the trained thresholds of a single H5 file

## Usage

``` r
read_thresholds_file(h5_path)
```

## Arguments

- h5_path:

  Path to an H5 classification file.

## Value

A named numeric vector (zero-length for an empty file), or `NULL` if the
file cannot be read or lacks `class_labels`/`thresholds`.
