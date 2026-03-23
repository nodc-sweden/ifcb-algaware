# Read classifier name from an H5 classification file

Extracts the `classifier_name` attribute from the first available H5
file in a directory.

## Usage

``` r
read_classifier_name(h5_dir)
```

## Arguments

- h5_dir:

  Directory containing .h5 files.

## Value

Character string with the classifier name, or NULL if unavailable.
