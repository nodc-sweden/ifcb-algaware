# Normalise character columns of a data frame to UTF-8

After reading a text file with an explicit `encoding`, this re-encodes
every character column to UTF-8 so downstream joins, matches and report
output behave identically regardless of the R session's native locale
(e.g. a non-UTF-8 Windows Server, where Å/Ä/Ö would otherwise be mangled
and silently drop rows). Strings already in UTF-8 are left unchanged.

## Usage

``` r
as_utf8_columns(df)
```

## Arguments

- df:

  A data.frame.

## Value

The data.frame with all character columns marked/encoded as UTF-8.
