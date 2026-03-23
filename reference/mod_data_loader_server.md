# Data Loader Module Server

Handles the full data loading pipeline: fetch metadata from the IFCB
Dashboard, match bins to monitoring stations, download raw files and
classifications, compute biovolumes, and populate the shared reactive
state (`rv`).

## Usage

``` r
mod_data_loader_server(id, config, rv)
```

## Arguments

- id:

  Module namespace ID.

- config:

  Reactive values with settings.

- rv:

  Reactive values for app state (see server.R for field docs).

## Value

NULL (side effects only).
