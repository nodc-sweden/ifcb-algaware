# Compute presence categories based on percentage of total counts

Categories follow the SHARK/HELCOM abundance scale: 5 = dominant
(\>=50%), 4 = abundant (\>=10%), 3 = common (\>=1%), 2 = scarce
(\>=0.1%), 1 = rare (\>0%), 0 = absent.

## Usage

``` r
compute_presence_categories(agg)
```

## Arguments

- agg:

  Data.frame with visit_id, STATION_NAME, counts_per_liter.

## Value

The input data.frame with added pct and Presence_cat columns.
