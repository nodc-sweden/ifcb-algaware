# Load the bundled taxa lookup table

Returns the pre-built mapping from classifier class names to WoRMS
scientific names and AphiaIDs.

## Usage

``` r
load_taxa_lookup()
```

## Value

A data.frame with columns `clean_names`, `name`, `AphiaID`.
