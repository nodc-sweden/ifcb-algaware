# Fetch image count metadata from the IFCB Dashboard

Retrieves per-sample metadata including image counts and coordinates
from the dashboard's export_metadata API endpoint. This is a lightweight
call that does not download any raw data files.

## Usage

``` r
fetch_image_counts(dashboard_url, dataset_name, start_date, end_date)
```

## Arguments

- dashboard_url:

  Dashboard base URL (e.g. "https://ifcb.example.com").

- dataset_name:

  Dataset name (e.g. "RV_Svea").

- start_date:

  Start date (Date or character yyyy-mm-dd).

- end_date:

  End date (Date or character yyyy-mm-dd).

## Value

A data.frame with columns: pid, sample_time, latitude, longitude,
n_images, ml_analyzed. Returns empty data.frame on failure.
