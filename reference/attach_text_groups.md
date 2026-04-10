# Attach collapsed text groups to station summary data

Attach collapsed text groups to station summary data

## Usage

``` r
attach_text_groups(x, phyto_groups = NULL)
```

## Arguments

- x:

  Station-level data frame with `name` and optionally `AphiaID`.

- phyto_groups:

  Group assignment table passed through
  `collapse_phyto_groups_for_text()`.

## Value

`x` with an added `text_group` column.
