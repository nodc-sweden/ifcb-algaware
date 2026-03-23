# Parse image IDs into sample_name and roi_number

Splits composite image IDs (e.g. `"D20221023T000155_IFCB134_00042"`)
back into their sample_name and roi_number components.

## Usage

``` r
parse_image_ids(img_ids)
```

## Arguments

- img_ids:

  Character vector of image IDs.

## Value

A data.frame with `sample_name` and `roi_number` columns.
