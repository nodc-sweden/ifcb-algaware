# Compute per-liter concentrations from aggregated data

Compute per-liter concentrations from aggregated data

## Usage

``` r
compute_per_liter(agg)
```

## Arguments

- agg:

  Data.frame with total_counts, total_biovolume_mm3, total_carbon_ug,
  total_ml_analyzed columns.

## Value

The input data.frame with added counts_per_liter,
biovolume_mm3_per_liter, carbon_ug_per_liter columns.
