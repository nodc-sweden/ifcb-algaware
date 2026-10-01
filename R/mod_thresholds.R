# Step of the class threshold slider. Values within half a step of the
# threshold in effect count as unchanged (see resolve_slider_value()).
threshold_slider_step <- 0.01

#' Resolve a slider value against the threshold in effect
#'
#' A slider with a fixed step cannot land exactly on a trained threshold such
#' as 0.7224, so a value within half a step of the threshold in effect is
#' taken to mean that threshold.
#'
#' @param value Slider value, or \code{NULL} before the slider exists.
#' @param effective The threshold currently in effect for the class.
#' @param step Slider step.
#' @return \code{effective} when \code{value} is within half a step of it,
#'   otherwise \code{value}.
#' @keywords internal
resolve_slider_value <- function(value, effective, step) {
  if (is.null(value)) return(NULL)
  if (abs(value - effective) <= step / 2 + 1e-9) effective else value
}

#' Sample IDs of the active (non-excluded) samples
#'
#' @param rv Reactive values with \code{matched_metadata_all} and
#'   \code{excluded_samples}.
#' @return Character vector of sample IDs, or \code{NULL} (all samples) when
#'   no metadata is loaded.
#' @keywords internal
active_sample_ids <- function(rv) {
  if (is.null(rv$matched_metadata_all)) return(NULL)
  setdiff(unique(rv$matched_metadata_all$pid), rv$excluded_samples)
}

#' Keep the current class index within the region's class list
#'
#' @param rv Reactive values used by \code{get_region_context()}.
#' @return \code{NULL}, invisibly.
#' @keywords internal
clamp_current_class_idx <- function(rv) {
  ctx <- get_region_context(rv)
  rv$current_class_idx <- max(1L, min(rv$current_class_idx,
                                      length(ctx$classes)))
  invisible(NULL)
}

#' Test whether two threshold adjustment vectors are the same
#'
#' @param a,b Named numeric vectors (possibly empty or \code{NULL}).
#' @return \code{TRUE} when both hold the same classes and values.
#' @keywords internal
same_adjustments <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)
  if (length(a) == 0) return(TRUE)
  setequal(names(a), names(b)) && all(a[names(b)] == b)
}

#' Format a threshold for display
#'
#' @param x Numeric threshold.
#' @return Character string with two decimals.
#' @keywords internal
format_threshold <- function(x) sprintf("%.2f", x)

#' Class Thresholds Module UI
#'
#' Slider for adjusting the classifier threshold of the class shown in the
#' gallery, with a live preview, and a list of the adjusted classes.
#'
#' @param id Module namespace ID.
#' @return A UI element.
#' @export
mod_thresholds_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::hr(),
    shiny::div(class = "small fw-semibold mb-1",
               shiny::icon("sliders"), " Class threshold"),
    shiny::uiOutput(ns("controls")),
    shiny::uiOutput(ns("preview")),
    shiny::uiOutput(ns("adjusted"))
  )
}

#' Class Thresholds Module Server
#'
#' Adjusting a threshold re-applies the ifcb-classify rule for that class to
#' every loaded sample (thresholds are per class, not per region): images
#' whose top class it is get the class when their score reaches the new
#' threshold, otherwise \code{"unclassified"}. Manual corrections always take
#' precedence, and threshold changes are never logged as corrections or saved
#' as annotations.
#'
#' While the slider is moved, the images that would leave the current class
#' are published in \code{rv$threshold_dimmed} for the gallery to dim.
#'
#' @param id Module namespace ID.
#' @param rv Reactive values for app state.
#' @return NULL (side effects only).
#' @export
mod_thresholds_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    step <- threshold_slider_step

    # The gallery's current class and whether it has a trained threshold
    current_target <- shiny::reactive({
      shiny::req(rv$data_loaded, rv$classifications)
      trained <- rv$thresholds_trained
      cls <- get_region_context(rv)$current_class
      list(
        class = cls,
        trained = trained,
        available = !is.null(trained) && !is.null(cls) &&
          cls %in% names(trained)
      )
    })

    effective_threshold <- function(cls) {
      adjustments <- rv$threshold_adjustments
      if (cls %in% names(adjustments)) {
        adjustments[[cls]]
      } else {
        rv$thresholds_trained[[cls]]
      }
    }

    # Threshold the slider asks for, or NULL when there is nothing to adjust
    requested_value <- function(target) {
      if (!target$available || is.null(input$threshold)) return(NULL)
      resolve_slider_value(input$threshold, effective_threshold(target$class),
                           step)
    }

    preview <- shiny::debounce(shiny::reactive({
      target <- current_target()
      value <- requested_value(target)
      if (is.null(value) || value == effective_threshold(target$class)) {
        return(NULL)
      }
      preview_threshold(rv$classifications_original,
                        rv$threshold_adjustments, rv$corrections,
                        target$class, value,
                        samples = active_sample_ids(rv))
    }), 300)

    shiny::observe({
      p <- preview()
      rv$threshold_dimmed <- if (is.null(p)) character(0) else p$removed
    })

    # Recompose the working classifications under new adjustments. Returns
    # FALSE when nothing changes.
    commit_adjustments <- function(new_adjustments) {
      if (same_adjustments(new_adjustments, rv$threshold_adjustments)) {
        return(FALSE)
      }
      composed <- compose_classifications(
        rv$classifications_original, new_adjustments, rv$corrections,
        active_sample_ids(rv)
      )
      rv$classifications_all <- composed$all
      rv$classifications <- composed$active
      rv$threshold_adjustments <- new_adjustments
      rv$threshold_dimmed <- character(0)
      rv$selected_images <- character(0)
      rv$summaries_stale <- TRUE
      clamp_current_class_idx(rv)
      TRUE
    }

    reset_class <- function(cls) {
      new_adjustments <- set_threshold_adjustment(
        rv$threshold_adjustments, cls, NULL, rv$thresholds_trained
      )
      if (commit_adjustments(new_adjustments)) {
        shiny::showNotification(
          paste0("Threshold for '", cls, "' reset to the trained ",
                 format_threshold(rv$thresholds_trained[[cls]]), "."),
          type = "message"
        )
      }
    }

    shiny::observeEvent(input$apply, {
      target <- current_target()
      value <- requested_value(target)
      if (is.null(value)) return()

      counts <- preview_threshold(
        rv$classifications_original, rv$threshold_adjustments,
        rv$corrections, target$class, value, samples = active_sample_ids(rv)
      )
      new_adjustments <- set_threshold_adjustment(
        rv$threshold_adjustments, target$class, value, rv$thresholds_trained,
        tolerance = step / 2
      )
      if (!commit_adjustments(new_adjustments)) return()

      shiny::showNotification(
        paste0("Threshold for '", target$class, "' set to ",
               format_threshold(value), ": ", counts$n_removed,
               " image(s) moved to unclassified, ", counts$n_added,
               " returned."),
        type = "message"
      )
    })

    shiny::observeEvent(input$reset, {
      target <- current_target()
      if (target$available) reset_class(target$class)
    })

    shiny::observeEvent(input$reset_class, {
      cls <- input$reset_class
      if (cls %in% names(rv$threshold_adjustments)) reset_class(cls)
    })

    shiny::observeEvent(input$reset_all, {
      if (commit_adjustments(numeric(0))) {
        shiny::showNotification("All class thresholds reset to trained values.",
                                type = "message")
      }
    })

    output$controls <- shiny::renderUI({
      target <- current_target()
      if (is.null(target$trained)) {
        return(shiny::p(class = "text-muted small mb-0",
                        "Class thresholds are not available in these ",
                        "classification files."))
      }
      if (!target$available) {
        return(shiny::p(class = "text-muted small mb-0",
                        "No trained threshold for this class."))
      }
      trained <- target$trained[[target$class]]
      current <- effective_threshold(target$class)
      is_adjusted <- target$class %in% names(rv$threshold_adjustments)

      shiny::tagList(
        shiny::sliderInput(ns("threshold"), NULL, min = 0, max = 1,
                           value = current, step = step, ticks = FALSE,
                           width = "100%"),
        shiny::p(
          class = "small text-muted mb-1",
          paste0("Trained: ", format_threshold(trained)),
          if (is_adjusted) {
            shiny::strong(paste0(" \u00b7 Applied: ", format_threshold(current)))
          }
        ),
        shiny::div(
          class = "d-flex gap-1",
          shiny::actionButton(ns("apply"), "Apply",
                              class = "btn-primary btn-sm flex-fill",
                              icon = shiny::icon("check")),
          shiny::actionButton(ns("reset"), "Reset",
                              class = "btn-outline-secondary btn-sm flex-fill",
                              icon = shiny::icon("rotate-left"))
        )
      )
    })

    output$preview <- shiny::renderUI({
      p <- preview()
      if (is.null(p)) return(NULL)
      if (p$n_removed == 0 && p$n_added == 0) {
        return(shiny::p(class = "small text-muted mt-1 mb-0",
                        "No images change at this threshold."))
      }
      parts <- c(
        if (p$n_removed > 0) {
          paste0(p$n_removed, " of ", p$n_current,
                 " image(s) move to unclassified (dimmed in the gallery)")
        },
        if (p$n_added > 0) {
          paste0(p$n_added, " unclassified image(s) join this class")
        }
      )
      shiny::p(class = "small text-warning-emphasis mt-1 mb-0",
               paste0(paste(parts, collapse = "; "),
                      ", across all active samples. Click Apply to keep."))
    })

    output$adjusted <- shiny::renderUI({
      adjustments <- rv$threshold_adjustments
      if (length(adjustments) == 0) return(NULL)
      trained <- rv$thresholds_trained

      rows <- lapply(names(adjustments), function(cls) {
        shiny::div(
          class = "d-flex justify-content-between align-items-center small",
          shiny::span(paste0(cls, ": ", format_threshold(trained[[cls]]),
                             " \u2192 ", format_threshold(adjustments[[cls]]))),
          shiny::tags$a(
            href = "#", title = "Reset to trained threshold",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
              ns("reset_class"), encodeString(cls, quote = '"')
            ),
            shiny::icon("rotate-left")
          )
        )
      })

      shiny::tagList(
        shiny::div(class = "small fw-semibold mt-2", "Adjusted thresholds"),
        rows,
        shiny::actionLink(ns("reset_all"), "Reset all", class = "small")
      )
    })
  })
}
