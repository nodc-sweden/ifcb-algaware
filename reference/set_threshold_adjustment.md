# Set or reset the threshold adjustment of one class

Set or reset the threshold adjustment of one class

## Usage

``` r
set_threshold_adjustment(
  adjustments,
  class_name,
  value,
  trained,
  tolerance = 1e-06
)
```

## Arguments

- adjustments:

  Named numeric vector of current adjustments (may be empty or `NULL`).

- class_name:

  Class to adjust. Must have a trained threshold.

- value:

  New threshold between 0 and 1, or `NULL` to reset the class to its
  trained threshold.

- trained:

  Named numeric vector of trained thresholds.

- tolerance:

  A `value` within this distance of the trained threshold counts as a
  reset.

## Value

The updated named numeric vector of adjustments. Classes back at their
trained threshold are dropped, so the vector only ever holds real
adjustments.
