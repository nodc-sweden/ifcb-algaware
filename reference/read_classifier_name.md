# Read classifier name from an H5 classification file

Extracts the `classifier_name` dataset from the first H5 file in a
directory that has one (empty files from bins without images do not).

## Usage

``` r
read_classifier_name(h5_dir, sample_ids = NULL)
```

## Arguments

- h5_dir:

  Directory containing .h5 files.

- sample_ids:

  Optional character vector of sample PIDs to consider. If NULL,
  considers all .h5 files in the directory.

## Value

Character string with the classifier name, or NULL if unavailable.
