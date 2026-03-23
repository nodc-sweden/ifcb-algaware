# Add a formatted paragraph to a Word document

Wrapper around
[`officer::body_add_fpar`](https://davidgohel.github.io/officer/reference/body_add_fpar.html)
that applies species name formatting when taxa_lookup is provided.

## Usage

``` r
add_formatted_par(doc, text, taxa_lookup = NULL, style = "Normal")
```

## Arguments

- doc:

  An `rdocx` object.

- text:

  Plain text string.

- taxa_lookup:

  Optional taxa lookup for formatting.

- style:

  Paragraph style name.

## Value

The modified `rdocx` object.
