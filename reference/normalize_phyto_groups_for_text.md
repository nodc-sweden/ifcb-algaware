# Normalize phytoplankton groups for report text

Standardises the group column name from either `phyto_group` or
`phyto_group.plankton_group` into `detailed_group` and `text_group`
(identical — all groups from the YAML config are preserved in both
columns).

## Usage

``` r
normalize_phyto_groups_for_text(phyto_groups)
```

## Arguments

- phyto_groups:

  Data frame with columns `name`, `AphiaID`, and either `phyto_group` or
  `phyto_group.plankton_group`.

## Value

Data frame with columns `name`, `AphiaID`, `detailed_group`, and
`text_group`.
