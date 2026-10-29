# Local folder for H5 files copied from a classification source

Each classification source (e.g. one folder per classifier version) gets
its own subfolder of `<storage>/classified_sources`, named after the
resolved source path. Copied files are never overwritten, so a shared
folder would keep serving results from a previously configured source
after the classification path is switched to another classifier. Without
a configured source, files are read from `<storage>/classified`; the
per-source folders live outside it because H5 files are searched
recursively.

## Usage

``` r
classification_cache_dir(storage, classification_path)
```

## Arguments

- storage:

  Local storage directory.

- classification_path:

  Classification source path as configured.

## Value

Path to the local folder.
