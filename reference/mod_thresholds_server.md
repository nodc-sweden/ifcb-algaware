# Class Thresholds Module Server

Adjusting a threshold re-applies the ifcb-classify rule for that class
to every loaded sample (thresholds are per class, not per region):
images whose top class it is get the class when their score reaches the
new threshold, otherwise `"unclassified"`. Manual corrections always
take precedence, and threshold changes are never logged as corrections
or saved as annotations.

## Usage

``` r
mod_thresholds_server(id, rv)
```

## Arguments

- id:

  Module namespace ID.

- rv:

  Reactive values for app state.

## Value

NULL (side effects only).

## Details

While the slider is moved, the images that would leave the current class
are published in `rv$threshold_dimmed` for the gallery to dim.
