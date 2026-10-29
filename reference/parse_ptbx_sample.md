# Parse the sheets of a Plankton Toolbox export

Parse the sheets of a Plankton Toolbox export

## Usage

``` r
parse_ptbx_sample(info, data, method, file_name)
```

## Arguments

- info:

  The `sample_info.txt` sheet (`key`/`value`).

- data:

  The `sample_data.txt` sheet.

- method:

  The `counting_method.txt` sheet.

- file_name:

  Name of the source file.

## Value

A data.frame with one row per taxon, sorted by abundance class (highest
first), with columns `sample_file`, `station_reported`, `sample_date`,
`latitude`, `longitude`, `min_depth_m`, `max_depth_m`, `sampler`,
`counting_method`, `analysed_by`, `name`, `display_name`, `taxon_class`,
`trophic_type`, `abundance_class` and `abundance_label`.
