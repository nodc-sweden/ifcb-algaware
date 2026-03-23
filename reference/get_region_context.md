# Get the current region context for validation

Resolves the current region samples, class list, index, and class name.
Shared by all validation actions to avoid duplication.

## Usage

``` r
get_region_context(rv)
```

## Arguments

- rv:

  Reactive values for app state.

## Value

A list with `region_samples`, `classes`, `idx`, and `current_class`
(NULL if no classes).
