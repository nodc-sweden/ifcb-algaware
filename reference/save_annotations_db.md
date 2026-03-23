# Save selected annotations to SQLite

Stores annotations for selected images. Compatible with ClassiPyR's
annotation format.

## Usage

``` r
save_annotations_db(
  db_path,
  annotations,
  annotator = "",
  class_list = character(0)
)
```

## Arguments

- db_path:

  Path to the SQLite database file.

- annotations:

  A data.frame with columns: `sample_name`, `roi_number`, `class_name`.

- annotator:

  Name of the annotator.

- class_list:

  Character vector of all class names (for class_lists table).

## Value

Logical TRUE on success, FALSE on failure.
