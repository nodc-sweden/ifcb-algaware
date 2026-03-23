# Resolve the active class list

Loads the global class list from the SQLite database (shared with
ClassiPyR). Returns NULL if the database does not exist or has no class
list entries.

## Usage

``` r
resolve_class_list(db_path)
```

## Arguments

- db_path:

  Path to the SQLite database file.

## Value

Character vector of class names, or NULL.
