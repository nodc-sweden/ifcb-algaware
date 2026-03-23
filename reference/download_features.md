# Download feature files for selected bins

Download feature files for selected bins

## Usage

``` r
download_features(
  dashboard_url,
  sample_ids,
  dest_dir,
  progress_callback = NULL
)
```

## Arguments

- dashboard_url:

  Dashboard base URL (must include dataset path for features).

- sample_ids:

  Character vector of sample PIDs.

- dest_dir:

  Destination directory.

- progress_callback:

  Optional progress callback.

## Value

Invisible NULL.
