# Read classifications from H5 files

Reads thresholded class assignments from H5 classification files
produced by the IFCB neural network classifier. Each H5 file contains:

- `roi_numbers`: integer vector of ROI (Region of Interest) IDs

- `class_name`: character vector of predicted class per ROI

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

A data.frame with columns: sample_name, roi_number, class_name, score.
