# Get HAB species names from taxa lookup

Get HAB species names from taxa lookup

## Usage

``` r
get_hab_species(taxa_lookup)
```

## Arguments

- taxa_lookup:

  A data.frame with columns `name` and `HAB`. If NULL or missing `HAB`
  column, returns empty character vector.

## Value

Character vector of scientific names flagged as HAB.
