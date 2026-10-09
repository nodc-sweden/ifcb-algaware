# Everything about a class's preview that does not depend on the slider

Finding the rows a class can hold and applying the current thresholds
and corrections to them takes a noticeable fraction of a second on a
full cruise. None of it changes while the slider moves, so it is done
once per class and reused for every slider value.

## Usage

``` r
preview_context(original, adjustments, corrections, class_name, samples = NULL)
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

- samples:

  Optional character vector of samples to count within (e.g. the current
  region). `NULL` counts all samples.

## Value

A list with, per candidate row, `sample_name`, `roi_number`, `score`,
`was` (currently labelled with the class) and `follows` (the label
follows this class's threshold: top class is the class, stored label
came from the threshold rule, and no manual correction).
