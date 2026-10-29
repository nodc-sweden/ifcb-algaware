#' Microscopy Samples Module UI
#'
#' Optional upload of qualitative Plankton Toolbox samples for stations
#' without IFCB data. Mounted inside the Report panel.
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @keywords internal
mod_microscopy_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "microscopy-section mb-2",
    shiny::uiOutput(ns("file_input")),
    shiny::uiOutput(ns("status"))
  )
}

#' Microscopy Samples Module Server
#'
#' Reads uploaded samples into \code{rv$microscopy_loaded} and shows which
#' of them fill a gap in the IFCB data. The gap selection is repeated at
#' report time against the corrected IFCB data.
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @param config Reactive values (or list) with settings.
#' @return NULL (side effects only).
#' @keywords internal
mod_microscopy_server <- function(id, rv, config) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Re-rendering the file input is the only way to clear its file list.
    input_version <- shiny::reactiveVal(0L)

    output$file_input <- shiny::renderUI({
      input_version()
      shiny::tagList(
        shiny::fileInput(ns("files"), "Microscopy samples (optional)",
                         multiple = TRUE, accept = ".xlsx",
                         placeholder = "Plankton Toolbox .xlsx"),
        shiny::helpText(
          class = "small mt-n2",
          "Qualitative samples for stations without IFCB data. They are",
          "described in the report text but not shown in the figures."
        )
      )
    })

    shiny::observeEvent(input$files, {
      files <- input$files
      rv$microscopy_loaded <- tryCatch(
        load_microscopy_samples(files$datapath, files$name,
                                load_algaware_stations(config$extra_stations)),
        error = function(e) {
          shiny::showNotification(
            paste("Could not load microscopy samples:", conditionMessage(e)),
            type = "error", duration = 10
          )
          NULL
        }
      )
    })

    shiny::observeEvent(input$clear, {
      rv$microscopy_loaded <- NULL
      input_version(input_version() + 1L)
    })

    output$status <- shiny::renderUI({
      loaded <- rv$microscopy_loaded
      if (is.null(loaded) || nrow(loaded$status) == 0) return(NULL)
      selected <- select_gap_samples(loaded, rv$station_summary)
      shiny::tagList(
        lapply(microscopy_status_items(selected$status), function(x) {
          shiny::div(
            class = "d-flex align-items-center gap-1",
            style = paste0("font-size: 12px; color: ",
                           if (x$ok) "green" else "#b35c00", ";"),
            shiny::icon(if (x$ok) "circle-check" else "circle-exclamation"),
            x$label
          )
        }),
        shiny::actionButton(ns("clear"), "Clear microscopy samples",
                            class = "btn-outline-secondary btn-sm mt-1")
      )
    })
  })
}

#' Status lines for loaded microscopy samples
#'
#' @param status The \code{status} table from \code{select_gap_samples()}.
#' @return List of items with \code{ok} (logical) and \code{label}.
#' @keywords internal
microscopy_status_items <- function(status) {
  if (is.null(status) || nrow(status) == 0) return(list())
  lapply(seq_len(nrow(status)), function(i) {
    what <- if (is.na(status$station[i])) {
      status$file[i]
    } else {
      paste(status$station[i], status$date[i])
    }
    list(
      ok = isTRUE(status$used[i]),
      label = if (isTRUE(status$used[i])) {
        paste0(what, ": used")
      } else {
        paste0(what, ": skipped, ", status$reason[i])
      }
    )
  })
}

#' Select and enrich the microscopy samples for a report
#'
#' @param loaded Result of \code{load_microscopy_samples()}, or NULL.
#' @param station_summary IFCB station summary used for the report.
#' @param taxa_lookup Taxa lookup used for the report.
#' @return A list with \code{obs} (enriched observations, or NULL when no
#'   sample fills a gap) and \code{notes} (messages for the user).
#' @keywords internal
prepare_report_microscopy <- function(loaded, station_summary, taxa_lookup) {
  none <- list(obs = NULL, notes = character(0))
  if (is.null(loaded) || nrow(loaded$obs) == 0) return(none)
  selected <- select_gap_samples(loaded, station_summary)
  if (nrow(selected$obs) == 0) return(none)
  enrich_microscopy_taxa(selected$obs, taxa_lookup)
}

#' Cruise description covering the microscopy sample dates
#'
#' @param cruise_info Cruise description from the IFCB data.
#' @param sample_times IFCB sample times of the cruise.
#' @param microscopy Microscopy observations used in the report, or NULL.
#' @return \code{cruise_info}, rebuilt with a date range that includes the
#'   microscopy samples when there are any.
#' @keywords internal
report_cruise_info <- function(cruise_info, sample_times, microscopy) {
  if (is.null(microscopy) || nrow(microscopy) == 0) return(cruise_info)
  build_cruise_info(c(as.Date(sample_times), microscopy$visit_date))
}
