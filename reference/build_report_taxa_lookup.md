# Extend the taxa lookup with microscopy taxa for one report

Italics, HAB asterisks and binomial abbreviation in the report text are
all driven by `taxa_lookup`. Appending the microscopy taxa to a
report-only copy makes them work for microscopy stations without
touching the formatting code. Taxa already in the lookup keep their
curated row.

## Usage

``` r
build_report_taxa_lookup(taxa_lookup, microscopy_obs)
```

## Arguments

- taxa_lookup:

  IFCB taxa lookup, or NULL.

- microscopy_obs:

  Enriched microscopy observations (from
  [`enrich_microscopy_taxa()`](https://nodc-sweden.github.io/ifcb-algaware/reference/enrich_microscopy_taxa.md)),
  or NULL.

## Value

`taxa_lookup` with one row added per new microscopy taxon (only the
microscopy rows when `taxa_lookup` is NULL).
