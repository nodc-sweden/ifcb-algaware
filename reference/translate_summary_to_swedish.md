# Translate an English summary to Swedish

Translate an English summary to Swedish

## Usage

``` r
translate_summary_to_swedish(
  english_text,
  provider = NULL,
  chl_measure = "fluorescence"
)
```

## Arguments

- english_text:

  Character string with the English summary to translate.

- provider:

  LLM provider (`"openai"` or `"gemini"`). NULL auto-detects.

- chl_measure:

  How the active chlorophyll source is measured, `"fluorescence"`
  (FerryBox/CTD) or `"concentration"` (LIMS bottle/hose filter samples).
  Adjusts the chlorophyll terminology used.

## Value

Character string with Swedish translation.
