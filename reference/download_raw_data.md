# Download raw IFCB files for selected bins

Downloads .roi, .adc, and .hdr files to local storage. Skips files that
already exist.

## Usage

``` r
download_raw_data(
  dashboard_url,
  sample_ids,
  dest_dir,
  progress_callback = NULL
)
```

## Arguments

- dashboard_url:

  Dashboard base URL.

- sample_ids:

  Character vector of sample PIDs.

- dest_dir:

  Destination directory.

- progress_callback:

  Optional function(current, total, message) for progress updates.

## Value

Invisible NULL.
