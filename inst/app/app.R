# AlgAware Shiny Application
# ===========================
# Entry point when running via algaware::launch_app() or shinyAppDir().
#
# Architecture overview:
#   ui.R     - Page layout: sidebar (settings, data loader, validation, report)
#              and main panel (gallery, maps, plots, summary table).
#   server.R - Shared reactive state (rv) and output renderers. Delegates
#              most logic to Shiny modules defined in R/mod_*.R.
#
# Shiny modules (in R/):
#   mod_settings     - Configuration (dashboard URL, paths, extra stations)
#   mod_data_loader  - Fetch metadata, download files, process data
#   mod_gallery      - Image browsing with class navigation and selection
#   mod_validation   - Annotation storage, relabeling, invalidation
#   mod_report       - Word report generation with optional LLM text
#
# Non-module R files handle the underlying logic:
#   data_download.R    - IFCB Dashboard API and file I/O
#   data_processing.R  - Biovolume computation and station aggregation
#   database.R         - SQLite annotation storage (ClassiPyR-compatible)
#   plots.R            - Heatmaps, maps, stacked bar charts
#   mosaics.R          - Adaptive image mosaic layout
#   llm.R              - LLM text generation for reports (OpenAI/Gemini/Claude)
#   report.R           - Word document assembly (officer package)
#   stations.R         - Spatial matching of samples to monitoring stations
#   utils.R            - Settings persistence, helper functions

library(algaware)
library(shiny)
library(bslib)
library(DT)

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui, server)
