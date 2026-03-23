# Copy classification H5 files for selected bins

Locates `*_class.h5` files by constructing direct paths from sample IDs
rather than listing the full directory tree. The classification path is
expected to contain yearly subfolders (e.g. `class2024_v3`). Only the
subfolder matching each sample's year is searched.

## Usage

``` r
copy_classification_files(
  classification_path,
  sample_ids,
  dest_dir,
  progress_callback = NULL
)
```

## Arguments

- classification_path:

  Source directory containing yearly subfolders with .h5 files.

- sample_ids:

  Character vector of sample PIDs (e.g. `"D20221023T000155_IFCB134"`).

- dest_dir:

  Destination directory.

- progress_callback:

  Optional progress callback.

## Value

Invisible character vector of copied file paths.
