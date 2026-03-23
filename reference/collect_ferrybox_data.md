# Collect ferrybox data for station visits

Collect ferrybox data for station visits

## Usage

``` r
collect_ferrybox_data(sample_times, ferrybox_path)
```

## Arguments

- sample_times:

  POSIXct vector of sample timestamps.

- ferrybox_path:

  Path to ferrybox data folder.

## Value

A data.frame with chlorophyll and other measurements per timestamp.
