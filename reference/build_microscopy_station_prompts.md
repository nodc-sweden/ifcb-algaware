# Build the prompts for a microscopy station description

Build the prompts for a microscopy station description

## Usage

``` r
build_microscopy_station_prompts(
  obs,
  all_microscopy = NULL,
  chl_measure = "fluorescence"
)
```

## Arguments

- obs:

  Enriched microscopy observations for one station visit.

- all_microscopy:

  Observations of all microscopy stations of the cruise, used as
  comparison context (only other microscopy stations are comparable).

- chl_measure:

  Chlorophyll measurement terminology key.

## Value

A list with `system` and `user` prompt strings.
