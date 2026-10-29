# Read the trained per-class thresholds from H5 classification files

Reads `class_labels` and `thresholds` from each H5 file. All files must
carry identical thresholds (the same classifier), since the threshold
adjustment feature works on one threshold per class.

## Usage

``` r
read_thresholds(h5_dir, sample_ids = NULL)
```

## Arguments

- h5_dir:

  Directory containing .h5 files.

- sample_ids:

  Optional character vector of sample PIDs to read. If NULL, reads all
  .h5 files in the directory.

## Value

A named numeric vector (class name -\> trained threshold), or `NULL`
when there are no non-empty files, any of them lacks the thresholds, or
they disagree (with a warning in that last case).

## Details

Empty files (bins without images) carry no thresholds and are ignored.
