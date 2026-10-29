# Microscopy Samples Module Server

Reads uploaded samples into `rv$microscopy_loaded` and shows which of
them fill a gap in the IFCB data. The gap selection is repeated at
report time against the corrected IFCB data.

## Usage

``` r
mod_microscopy_server(id, rv, config)
```

## Arguments

- id:

  Module namespace ID.

- rv:

  Reactive values for app state.

- config:

  Reactive values (or list) with settings.

## Value

NULL (side effects only).
