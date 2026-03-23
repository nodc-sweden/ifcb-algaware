# Launch the AlgAware Shiny Application

Start the interactive AlgAware application for IFCB phytoplankton data
processing, validation, and report generation.

## Usage

``` r
launch_app(launch.browser = TRUE, reset_settings = FALSE, ...)
```

## Arguments

- launch.browser:

  Logical; open the app in a browser? Default TRUE.

- reset_settings:

  Logical; if TRUE, reset all settings to defaults before launching.
  Default FALSE.

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

A Shiny app object (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
  algaware::launch_app()
  algaware::launch_app(reset_settings = TRUE)
} # }
```
