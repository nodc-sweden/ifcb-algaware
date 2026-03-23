# Summarize biovolumes for classified bins

Wraps
[`iRfcb::ifcb_summarize_biovolumes()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_summarize_biovolumes.html)
and joins with taxonomy.

## Usage

``` r
summarize_biovolumes(
  feature_folder,
  hdr_folder,
  classifications,
  taxa_lookup,
  non_bio_classes = character(0),
  pixels_per_micron = 2.77
)
```

## Arguments

- feature_folder:

  Path to the feature CSV directory.

- hdr_folder:

  Path to the raw data directory (for .hdr files).

- classifications:

  A data.frame from
  [`read_h5_classifications()`](https://anderstorstensson.github.io/shiny-ifcb-algaware/reference/read_h5_classifications.md).

- taxa_lookup:

  A data.frame with columns `clean_names`, `name`, `AphiaID`.

- non_bio_classes:

  Character vector of non-biological class names to exclude.

- pixels_per_micron:

  Conversion factor from pixels to microns. Default 2.77, which is the
  optical calibration constant for the standard IFCB instrument.
  Different IFCB units may use slightly different values.

## Value

A data.frame with per-sample, per-class biovolume data joined with
taxonomy.
