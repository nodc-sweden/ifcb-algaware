# Build a formatted paragraph with italic species names and red HAB asterisks

Parses plain text and returns an
[`officer::fpar`](https://davidgohel.github.io/officer/reference/fpar.html)
object with italic formatting for recognized species/genus names and red
bold formatting for HAB asterisks.

## Usage

``` r
format_report_paragraph(text, taxa_lookup = NULL)
```

## Arguments

- text:

  Character string of plain text.

- taxa_lookup:

  Data frame with columns `name`, `italic`, and optionally `HAB`.

## Value

An
[`officer::fpar`](https://davidgohel.github.io/officer/reference/fpar.html)
object.
