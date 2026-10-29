# Read a qualitative Plankton Toolbox sample

Reads one Plankton Toolbox Excel export with
[`SHARK4R::read_ptbx()`](https://sharksmhi.github.io/SHARK4R/reference/read_ptbx.html)
and returns one row per observed taxon with the sample metadata repeated
on every row.

## Usage

``` r
read_microscopy_sample(path, file_name = basename(path))
```

## Arguments

- path:

  Path to the Plankton Toolbox `.xlsx` export.

- file_name:

  Name to report the sample under (defaults to the file name; Shiny
  uploads use the original name of the uploaded file).

## Value

A data.frame, see
[`parse_ptbx_sample()`](https://nodc-sweden.github.io/ifcb-algaware/reference/parse_ptbx_sample.md).
