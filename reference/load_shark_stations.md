# Load SHARK station bundle (internal wrapper)

Wraps the internal `SHARK4R:::load_station_bundle()` call to centralise
the dependency and provide a fallback.

## Usage

``` r
load_shark_stations(verbose = FALSE)
```

## Arguments

- verbose:

  Passed to `load_station_bundle`.

## Value

A data.frame of SHARK stations, or an empty data.frame on failure.
