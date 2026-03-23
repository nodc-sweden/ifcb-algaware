# Build a descriptive cruise info string from sample timestamps

Determines the most common month among samples and formats as "RV Svea
cruise, to ".

## Usage

``` r
build_cruise_info(sample_times)
```

## Arguments

- sample_times:

  POSIXct vector of sample timestamps.

## Value

Character string.
