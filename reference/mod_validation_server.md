# Validation Module Server

Provides four validation actions:

1.  **Store Annotations**: save selected images to the SQLite database
    (persistent, shared with ClassiPyR)

2.  **Relabel Selected**: move selected images to a different class
    (session-only, logged in rv\$corrections)

3.  **Relabel Class**: move ALL images of the current class in the
    current region to a different class (session-only)

4.  **Invalidate Class**: mark an entire class as non-biological /
    unclassified (session-only)

## Usage

``` r
mod_validation_server(id, rv, config)
```

## Arguments

- id:

  Module namespace ID.

- rv:

  Reactive values for app state.

- config:

  Reactive values with settings.

## Value

NULL (side effects only).
