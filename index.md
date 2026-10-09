# AlgAware-IFCB

[![R-CMD-check](https://github.com/nodc-sweden/ifcb-algaware/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nodc-sweden/ifcb-algaware/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-brightgreen.svg)](https://nodc-sweden.github.io/ifcb-algaware/)

AlgAware-IFCB is the R/Shiny app used at SMHI to turn Imaging
FlowCytobot (IFCB) data from a monitoring cruise into the AlgAware
phytoplankton report. You load a cruise from the IFCB Dashboard, go
through the classifier’s predictions in an image gallery and correct
what it got wrong. The app then builds a Word report with maps,
heatmaps, image mosaics and a description of each station.

The app is written for the Swedish national marine monitoring programme
and assumes the SMHI setup: samples from R/V Svea, an IFCB Dashboard on
the internal network and the twelve AlgAware stations. Other institutes
running an IFCB can adapt it, but the station files and data paths will
need changing.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("nodc-sweden/ifcb-algaware",
                        dependencies = TRUE,
                        ref = remotes::github_release())
```

`dependencies = TRUE` also installs the suggested packages. The CTD
figures need `oce` and `patchwork`, and the maps need
`rnaturalearthdata`. On Linux a few system libraries have to be
installed first. They are listed in the [installation
guide](https://nodc-sweden.github.io/ifcb-algaware/articles/installation.html).

## Using the app

``` r

library(algaware)
launch_app()
```

The app opens in your browser. The first time, fill in the Settings
panel. After that a cruise goes roughly like this:

1.  Fetch metadata from the Dashboard and pick a cruise number or a date
    range. The app matches bins to stations and downloads what it needs.
    Files are cached locally, so loading the same cruise again is quick.
2.  Step through every class in the gallery, for the Baltic Sea and for
    the West Coast. Relabel or unclassify the images that are wrong. A
    class you leave alone counts as accepted.
3.  If you have them, load CTD casts and LIMS chlorophyll in the CTD
    tab, and choose images for the front-page mosaics.
4.  Make the report and download the `.docx`. Download the corrections
    log as well and archive it with the report.

The [workflow
guide](https://nodc-sweden.github.io/ifcb-algaware/articles/workflow.html)
covers each step with screenshots. Cruise numbers only appear once the
year’s metadata file has been uploaded to the Dashboard, which is
described in [its own
guide](https://nodc-sweden.github.io/ifcb-algaware/articles/ifcb-dashboard-metadata.html).

Manual annotations are saved to an SQLite file in the same format as
ClassiPyR, so the two tools can share a database.

## Data the app reads

| Data | Where it comes from | Required |
|----|----|----|
| Sample metadata, raw files (`.roi`, `.adc`, `.hdr`) and feature files | IFCB Dashboard | yes |
| Classifier output (`.h5`) | Classification Path in Settings | yes |
| FerryBox chlorophyll fluorescence (`.txt`) | FerryBox Data Path in Settings | no |
| CTD casts (`.cnv`) | Folder given in the CTD tab | no |
| LIMS chlorophyll export (`data.txt`) | File chosen in the CTD tab | no |

Downloads are cached under the Local Storage Path, which defaults to
`./algaware_data`. Settings are edited in the app and saved to
`settings.json` in the R user config directory
(`tools::R_user_dir("algaware", "config")`). The installation guide has
the full list of settings.

## AI-written report text

The Swedish and English summaries and the station descriptions can be
drafted by a language model. This is switched off unless one of these
environment variables is set:

| Variable            | Provider         | Default model           |
|---------------------|------------------|-------------------------|
| `OPENAI_API_KEY`    | OpenAI           | `gpt-5.1`               |
| `GEMINI_API_KEY`    | Google Gemini    | `gemini-2.5-flash-lite` |
| `ANTHROPIC_API_KEY` | Anthropic Claude | `claude-opus-5-5`       |

Override the model with `OPENAI_MODEL`, `GEMINI_MODEL` or
`ANTHROPIC_MODEL`. When several keys are set, OpenAI is used by default
and the provider can be switched in the Report tab.

The app sends station-level summaries (taxa, biovolume, cell counts,
mean chlorophyll) together with the writing guide in
`inst/extdata/report_writing_guide.md`. It does not send images,
positions or depths. Without a key, the report has placeholder text
where the summaries would be.

## Adapting it

The station lists, the taxa lookup (names, AphiaIDs, HAB flags), the
phytoplankton groups, the Word template and the writing guide are plain
files under `inst/`. The [installation
guide](https://nodc-sweden.github.io/ifcb-algaware/articles/installation.html#bundled-configuration-files)
and [Customising the
Report](https://nodc-sweden.github.io/ifcb-algaware/articles/report-customisation.html)
explain what each file does and how to edit it.

## Development

``` r

devtools::test()
```

The exported functions are documented in the
[reference](https://nodc-sweden.github.io/ifcb-algaware/reference/index.html).
Report bugs in the [issue
tracker](https://github.com/nodc-sweden/ifcb-algaware/issues).

## License

MIT
