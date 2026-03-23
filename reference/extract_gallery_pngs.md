# Extract missing PNGs for gallery display

Checks which ROIs are already extracted and only extracts missing ones.

## Usage

``` r
extract_gallery_pngs(imgs, raw_dir, png_dir)
```

## Arguments

- imgs:

  Data.frame with sample_name and roi_number columns.

- raw_dir:

  Path to raw data directory.

- png_dir:

  Path to PNG output directory.

## Value

Invisible NULL.
