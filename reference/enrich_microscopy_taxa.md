# Add HAB flags, italics and phytoplankton groups to microscopy taxa

Uses WoRMS (AphiaID and rank), the IOC-UNESCO HAB list and
[`assign_phyto_groups()`](https://nodc-sweden.github.io/ifcb-algaware/reference/assign_phyto_groups.md),
so groups match those of the IFCB taxa. When a service is unavailable it
falls back to offline rules: groups from the Plankton Toolbox taxon
class via `phyto_groups.yaml`, italics from the name, and HAB flags from
`taxa_lookup` only.

## Usage

``` r
enrich_microscopy_taxa(obs, taxa_lookup = NULL)
```

## Arguments

- obs:

  Microscopy observations with `name` and `taxon_class`.

- taxa_lookup:

  Optional IFCB taxa lookup with `name` and `HAB`.

## Value

A list with `obs` (with `AphiaID`, `HAB`, `italic` and `phyto_group`
added) and `notes` (messages for the user about lookups that failed).

## Details

A taxon present in `taxa_lookup` keeps its curated HAB flag; other taxa
are flagged on an exact HAB-list match by name or AphiaID.
