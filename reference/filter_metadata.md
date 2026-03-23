# Filter metadata by cruise number or date range

Filter metadata by cruise number or date range

## Usage

``` r
filter_metadata(metadata, cruise = NULL, date_from = NULL, date_to = NULL)
```

## Arguments

- metadata:

  Dashboard metadata data.frame.

- cruise:

  Optional cruise number to filter on.

- date_from:

  Optional start date (Date or character yyyy-mm-dd).

- date_to:

  Optional end date.

## Value

Filtered metadata data.frame.
