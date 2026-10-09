# Read classifications from H5 files

Reads thresholded class assignments from H5 classification files
produced by the IFCB neural network classifier. Each H5 file contains:

- `roi_numbers`: integer vector of ROI (Region of Interest) IDs

- `class_name`: character vector of predicted class per ROI after
  applying the per-class thresholds (`"unclassified"` when the top score
  is below the threshold of the top class)

- `class_name_auto`: the top-scoring class per ROI, before thresholding

- `output_scores`: matrix of class probabilities (classes x ROIs); the
  maximum score per ROI is used as the confidence value

## Usage

``` r
read_h5_classifications(h5_dir, sample_ids = NULL)
```

## Arguments

- h5_dir:

  Directory containing .h5 files.

- sample_ids:

  Optional character vector of sample PIDs to read. If NULL, reads all
  .h5 files in the directory.

## Value

A data.frame with columns: sample_name, roi_number, class_name,
class_auto (top-scoring class before thresholding, `NA` when the file
does not provide it), score.
