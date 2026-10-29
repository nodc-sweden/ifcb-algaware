# Add station report sections to document

Microscopy stations (stations without IFCB data) are merged into the
same region and date order as the IFCB stations; their headings are
marked "(microscopy)".

## Usage

``` r
add_station_sections(
  doc,
  station_summary,
  taxa_lookup = NULL,
  use_llm = FALSE,
  phyto_groups = NULL,
  llm_provider = NULL,
  on_llm_progress = NULL,
  unclassified_fractions = NULL,
  chl_measure = "fluorescence",
  microscopy = NULL
)
```
