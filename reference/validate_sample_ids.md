# Validate IFCB sample IDs

Checks that sample IDs match the expected IFCB format (e.g.
`D20221023T000155_IFCB134`).

## Usage

``` r
validate_sample_ids(sample_ids)
```

## Arguments

- sample_ids:

  Character vector of sample IDs.

## Value

Invisible TRUE if all valid; stops with an error otherwise.
