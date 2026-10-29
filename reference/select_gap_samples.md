# Keep the microscopy samples that fill a gap in the IFCB data

IFCB data takes precedence: a sample is skipped when the loaded cruise
has IFCB data for its station. Samples dated more than `window_days`
outside the IFCB cruise dates (likely from another cruise) and second
samples for the same station are skipped too.

## Usage

``` r
select_gap_samples(loaded, station_summary, window_days = 7)
```

## Arguments

- loaded:

  Result of
  [`load_microscopy_samples()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_microscopy_samples.md).

- station_summary:

  IFCB station summary of the loaded cruise, or NULL to skip the IFCB
  checks.

- window_days:

  Allowed distance in days from the IFCB cruise dates.

## Value

`loaded` with `obs` restricted to the used samples and `status` updated
with the reasons for skipping.
