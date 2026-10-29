# Read and station-match a set of microscopy samples

A file that cannot be read, or whose station is not an AlgAware station,
is reported in `status` instead of stopping the whole load.

## Usage

``` r
load_microscopy_samples(
  paths,
  file_names = basename(paths),
  algaware_stations = load_algaware_stations(),
  shark_stations = load_shark_stations(verbose = FALSE)
)
```

## Arguments

- paths:

  Paths to Plankton Toolbox `.xlsx` exports.

- file_names:

  Names to report the files under.

- algaware_stations:

  Data frame from
  [`load_algaware_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_algaware_stations.md).

- shark_stations:

  SHARK station register.

## Value

A list with `obs` (one row per taxon and sample, with `STATION_NAME`,
`STATION_NAME_SHORT`, `COAST`, `visit_date`, `visit_id` and
`sample_index`) and `status` (one row per file: `sample_index`, `file`,
`station`, `date`, `used`, `reason`).
