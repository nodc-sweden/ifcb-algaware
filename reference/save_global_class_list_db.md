# Save global class list to SQLite

Replaces the contents of the `global_class_list` table. Compatible with
ClassiPyR's global_class_list table.

## Usage

``` r
save_global_class_list_db(db_path, class2use)
```

## Arguments

- db_path:

  Path to the SQLite database file.

- class2use:

  Character vector of class names.

## Value

Logical TRUE on success, FALSE on failure.
