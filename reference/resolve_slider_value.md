# Resolve a slider value against the threshold in effect

A slider with a fixed step cannot land exactly on a trained threshold
such as 0.7224, so a value within half a step of the threshold in effect
is taken to mean that threshold.

## Usage

``` r
resolve_slider_value(value, effective, step)
```

## Arguments

- value:

  Slider value, or `NULL` before the slider exists.

- effective:

  The threshold currently in effect for the class.

- step:

  Slider step.

## Value

`effective` when `value` is within half a step of it, otherwise `value`.
