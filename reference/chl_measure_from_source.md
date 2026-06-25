# Describe how the active chlorophyll source is measured

FerryBox and CTD report in-situ chlorophyll *fluorescence*, whereas the
LIMS bottle (`"lims"`) and hose (`"lims_hose"`, 0-10 m integrated)
samples are chlorophyll-a *concentrations* measured on a filter. The
report text must describe the value accordingly, so the wording follows
the source actually selected for the maps and figures.

## Usage

``` r
chl_measure_from_source(chl_map_source = "ferrybox")
```

## Arguments

- chl_map_source:

  Active chlorophyll map source: `"ferrybox"`, `"ctd"`, `"lims"` or
  `"lims_hose"`.

## Value

`"fluorescence"` or `"concentration"`.
