# Row of each correction in a classifications table

Matches on numeric keys (sample index and ROI number) instead of pasted
text keys, which dominated the run time on a full cruise.

## Usage

``` r
correction_row_index(classifications, corrections)
```

## Arguments

- classifications:

  A data.frame with `sample_name` and `roi_number` columns.

- corrections:

  A data.frame with `sample_name` and `roi_number` columns, or `NULL`.

## Value

Integer vector with one element per correction: the row it applies to,
or `NA` when the ROI is not in `classifications`. Empty when there are
no corrections.
