# Fetch metadata from the IFCB Dashboard

Wraps
[`iRfcb::ifcb_download_dashboard_metadata()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_download_dashboard_metadata.html)
and extracts available cruise numbers.

## Usage

``` r
fetch_dashboard_metadata(dashboard_url, dataset_name = NULL)
```

## Arguments

- dashboard_url:

  Dashboard base URL.

- dataset_name:

  Dataset name (e.g. "RV_Svea").

## Value

A list with `metadata` (data.frame) and `cruise_numbers` (character
vector, possibly empty if no cruise column exists).
