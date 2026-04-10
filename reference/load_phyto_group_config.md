# Load phytoplankton group configuration from YAML

Reads `inst/config/phyto_groups.yaml` and returns a list with two
elements: `core` (named list of class/phylum vectors for the three
built-in SHARK4R groups) and `custom` (named list suitable for the
`custom_groups` argument of
[`SHARK4R::assign_phytoplankton_group()`](https://sharksmhi.github.io/SHARK4R/reference/assign_phytoplankton_group.html)).

## Usage

``` r
load_phyto_group_config()
```

## Value

Named list with elements `core` and `custom`.
