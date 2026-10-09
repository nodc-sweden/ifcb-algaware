# Rows of the report's data summary table (Table 1)

Rows of the report's data summary table (Table 1)

## Usage

``` r
build_summary_rows(
  image_counts = NULL,
  n_station_samples = NULL,
  total_bio_images = NULL,
  classifier_name = NULL,
  threshold_adjustments = NULL,
  llm_model = NULL
)
```

## Arguments

- image_counts:

  Optional data frame of cruise-wide image counts.

- n_station_samples:

  Optional number of samples from AlgAware stations.

- total_bio_images:

  Optional number of biological images analysed.

- classifier_name:

  Optional classifier model name.

- threshold_adjustments:

  Optional data.frame from
  [`threshold_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/threshold_summary.md).

- llm_model:

  Optional LLM model name.

## Value

A data.frame with `Parameter` and `Value` columns.
