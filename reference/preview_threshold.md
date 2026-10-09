# Preview the effect of a proposed threshold for one class

Compares the working labels under the current adjustments with those
under the proposed threshold, both with manual corrections applied, so
manually corrected images are never counted.

## Usage

``` r
preview_threshold(
  original,
  adjustments,
  corrections,
  class_name,
  value,
  samples = NULL
)
```

## Arguments

- original:

  The load-time classifications.

- adjustments:

  Current named numeric vector of adjustments.

- corrections:

  Corrections log data.frame, or `NULL`.

- class_name:

  Class whose threshold is being changed.

- value:

  Proposed threshold.

- samples:

  Optional character vector of samples to count within (e.g. the current
  region). `NULL` counts all samples.

## Value

A list with integer counts `n_current` (images currently in the class),
`n_removed` (would move to unclassified) and `n_added` (would join the
class from unclassified), plus the image IDs
(`"<sample_name>_<roi_number>"`, as used by the gallery) of the moving
images in `removed` and `added`.

## Details

Convenience wrapper around
[`preview_context()`](https://nodc-sweden.github.io/ifcb-algaware/reference/preview_context.md)
and
[`preview_from_context()`](https://nodc-sweden.github.io/ifcb-algaware/reference/preview_from_context.md);
the app keeps the context between slider moves instead.
