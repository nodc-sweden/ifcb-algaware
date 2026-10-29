# Cruise description covering the microscopy sample dates

Cruise description covering the microscopy sample dates

## Usage

``` r
report_cruise_info(cruise_info, sample_times, microscopy)
```

## Arguments

- cruise_info:

  Cruise description from the IFCB data.

- sample_times:

  IFCB sample times of the cruise.

- microscopy:

  Microscopy observations used in the report, or NULL.

## Value

`cruise_info`, rebuilt with a date range that includes the microscopy
samples when there are any.
