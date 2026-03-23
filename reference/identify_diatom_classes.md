# Identify diatom classes from taxa lookup

Diatoms require a different biovolume formula than other phytoplankton
(they have silica frustules that affect the carbon:biovolume ratio).
This function matches class names against known diatom genera so that
[`iRfcb::ifcb_summarize_biovolumes()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_summarize_biovolumes.html)
can apply the correct formula.

## Usage

``` r
identify_diatom_classes(taxa_lookup)
```

## Arguments

- taxa_lookup:

  A data.frame with `clean_names` column.

## Value

Character vector of class names likely to be diatoms.
