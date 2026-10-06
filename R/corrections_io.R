#' Build the corrections CSV export, including threshold adjustments
#'
#' One file holds everything needed to restore a validation session: the
#' corrections log (with embedded custom class metadata, see
#' \code{enrich_corrections_for_export()}) followed by one row per adjusted
#' class threshold. A \code{record_type} column tells the two apart
#' (\code{"correction"} or \code{"threshold"}); threshold rows leave the
#' correction columns empty and fill \code{threshold_class},
#' \code{threshold_trained}, \code{threshold_adjusted} and
#' \code{threshold_n_moved}.
#'
#' @param corrections Corrections log data.frame (\code{rv$corrections}).
#' @param custom_classes Custom classes data.frame (\code{rv$custom_classes}).
#' @param thresholds Optional data.frame from \code{threshold_summary()}.
#' @return A data.frame ready for \code{utils::write.csv()}.
#' @keywords internal
build_corrections_export <- function(corrections, custom_classes,
                                     thresholds = NULL) {
  enriched <- enrich_corrections_for_export(corrections, custom_classes)
  n <- nrow(enriched)
  enriched$record_type <- rep("correction", n)
  enriched$threshold_class <- rep(NA_character_, n)
  enriched$threshold_trained <- rep(NA_real_, n)
  enriched$threshold_adjusted <- rep(NA_real_, n)
  enriched$threshold_n_moved <- rep(NA_integer_, n)

  if (is.null(thresholds) || nrow(thresholds) == 0) return(enriched)

  # Indexing with NA rows gives all-NA rows with the right column types
  threshold_rows <- enriched[rep(NA_integer_, nrow(thresholds)), ,
                             drop = FALSE]
  threshold_rows$record_type <- "threshold"
  threshold_rows$threshold_class <- thresholds$class_name
  threshold_rows$threshold_trained <- thresholds$trained
  threshold_rows$threshold_adjusted <- thresholds$adjusted
  threshold_rows$threshold_n_moved <- as.integer(thresholds$n_moved)

  result <- rbind(enriched, threshold_rows)
  rownames(result) <- NULL
  result
}

#' Split an imported corrections file into corrections and thresholds
#'
#' Files written before threshold adjustments existed have no
#' \code{record_type} column; all their rows are corrections.
#'
#' @param df Data.frame read from a corrections CSV.
#' @return A list with \code{corrections} and \code{thresholds} data.frames.
#' @keywords internal
split_corrections_import <- function(df) {
  if (!"record_type" %in% names(df)) {
    return(list(corrections = df, thresholds = df[0, , drop = FALSE]))
  }
  is_threshold <- !is.na(df$record_type) & df$record_type == "threshold"
  list(
    corrections = df[!is_threshold, , drop = FALSE],
    thresholds = df[is_threshold, , drop = FALSE]
  )
}

#' Columns an imported corrections file lacks
#'
#' The correction columns are always required. A file with threshold rows
#' must also have the columns \code{adjustments_from_import()} reads, which
#' a hand-edited or truncated file may have lost.
#'
#' @param df Data.frame read from a corrections CSV.
#' @return Character vector of the missing column names, empty when the file
#'   can be imported.
#' @keywords internal
missing_import_columns <- function(df) {
  required <- c("sample_name", "roi_number", "original_class", "new_class")
  if (nrow(split_corrections_import(df)$thresholds) > 0) {
    required <- c(required, "threshold_class", "threshold_trained",
                  "threshold_adjusted")
  }
  setdiff(required, names(df))
}

#' Turn imported threshold rows into threshold adjustments
#'
#' A row is only applied when its class exists in the loaded classifier and
#' its trained threshold matches the loaded one, so thresholds saved for a
#' different classifier are never applied. Rows that are skipped are
#' reported with the reason.
#'
#' @param threshold_rows Threshold rows from
#'   \code{split_corrections_import()}, or \code{NULL}.
#' @param trained Named numeric vector of trained thresholds, or \code{NULL}
#'   when the loaded files have none.
#' @param tolerance Allowed difference between the file's and the loaded
#'   trained thresholds.
#' @return A list with \code{adjustments} (named numeric vector) and
#'   \code{skipped} (data.frame with \code{class_name} and \code{reason}).
#' @keywords internal
adjustments_from_import <- function(threshold_rows, trained,
                                    tolerance = 1e-6) {
  adjustments <- numeric(0)
  skipped <- data.frame(class_name = character(0), reason = character(0),
                        stringsAsFactors = FALSE)
  if (is.null(threshold_rows) || nrow(threshold_rows) == 0) {
    return(list(adjustments = adjustments, skipped = skipped))
  }

  for (i in seq_len(nrow(threshold_rows))) {
    cls <- as.character(threshold_rows$threshold_class[i])
    reason <- threshold_import_problem(
      cls, as.numeric(threshold_rows$threshold_trained[i]),
      as.numeric(threshold_rows$threshold_adjusted[i]), trained, tolerance
    )
    if (is.null(reason)) {
      adjustments <- set_threshold_adjustment(
        adjustments, cls, as.numeric(threshold_rows$threshold_adjusted[i]),
        trained
      )
    } else {
      skipped <- rbind(skipped, data.frame(class_name = cls, reason = reason,
                                           stringsAsFactors = FALSE))
    }
  }
  list(adjustments = adjustments, skipped = skipped)
}

#' Check one imported threshold row
#'
#' @param cls Class name.
#' @param file_trained Trained threshold recorded in the file.
#' @param adjusted Adjusted threshold recorded in the file.
#' @param trained Named numeric vector of loaded trained thresholds, or
#'   \code{NULL}.
#' @param tolerance Allowed trained-threshold difference.
#' @return \code{NULL} when the row can be applied, otherwise the reason it
#'   cannot.
#' @keywords internal
threshold_import_problem <- function(cls, file_trained, adjusted, trained,
                                     tolerance) {
  if (is.null(trained)) {
    return("thresholds not available in the loaded classification files")
  }
  if (is.na(cls) || !cls %in% names(trained)) {
    return("class not in the loaded classifier")
  }
  if (is.na(file_trained) ||
      abs(file_trained - trained[[cls]]) > tolerance) {
    return("trained threshold differs (different classifier?)")
  }
  if (is.na(adjusted) || adjusted < 0 || adjusted > 1) {
    return("adjusted threshold is not between 0 and 1")
  }
  NULL
}

#' Threshold adjustment table for the current session
#'
#' @param rv Reactive values with \code{thresholds_trained},
#'   \code{threshold_adjustments} and \code{classifications_original}.
#' @return A data.frame from \code{threshold_summary()}, or \code{NULL} when
#'   no threshold is adjusted.
#' @keywords internal
current_threshold_table <- function(rv) {
  adjustments <- rv$threshold_adjustments
  if (length(adjustments) == 0 || is.null(rv$thresholds_trained)) {
    return(NULL)
  }
  threshold_summary(rv$thresholds_trained, adjustments,
                    rv$classifications_original)
}

#' Format threshold adjustments for the report summary table
#'
#' @param thresholds Data.frame from \code{threshold_summary()}, or
#'   \code{NULL}.
#' @return A single string such as \code{"Unicells (0.72 -> 0.82)"}, or
#'   \code{NULL} when there are no adjustments.
#' @keywords internal
format_threshold_adjustments <- function(thresholds) {
  if (is.null(thresholds) || nrow(thresholds) == 0) return(NULL)
  paste0(thresholds$class_name, " (", format_threshold(thresholds$trained),
         " \u2192 ", format_threshold(thresholds$adjusted), ")",
         collapse = "; ")
}

#' Body of the "Import Corrections" confirmation dialog
#'
#' @param corrections Correction rows from \code{split_corrections_import()}.
#' @param thresholds Result of \code{adjustments_from_import()}.
#' @param file_name Name of the imported file.
#' @param n_current Number of corrections in the current session.
#' @param n_current_thresholds Number of threshold adjustments in the current
#'   session.
#' @return A \code{shiny::tagList}.
#' @keywords internal
import_preview_ui <- function(corrections, thresholds, file_name, n_current,
                              n_current_thresholds) {
  relabels <- corrections[corrections$new_class != "unclassified", ,
                          drop = FALSE]
  invalidated <- unique(
    corrections$original_class[corrections$new_class == "unclassified"]
  )
  adjustments <- thresholds$adjustments
  skipped <- thresholds$skipped

  replaced <- c(
    if (n_current > 0) paste0(n_current, " existing correction(s)"),
    if (n_current_thresholds > 0) {
      paste0(n_current_thresholds, " threshold adjustment(s)")
    }
  )

  shiny::tagList(
    shiny::p(paste0("Import ", nrow(corrections), " correction(s) and ",
                    length(adjustments), " threshold adjustment(s) from '",
                    file_name, "'?")),
    if (nrow(relabels) > 0) {
      agg <- stats::aggregate(roi_number ~ original_class + new_class,
                              data = relabels, FUN = length)
      shiny::p(
        "Relabels: ",
        shiny::tags$ul(lapply(seq_len(nrow(agg)), function(i) {
          shiny::tags$li(paste0(agg$roi_number[i], "x ",
                                agg$original_class[i], " \u2192 ",
                                agg$new_class[i]))
        }))
      )
    },
    if (length(invalidated) > 0) {
      shiny::p(paste0("Unclassified: ", paste(invalidated, collapse = ", ")))
    },
    if (length(adjustments) > 0) {
      shiny::p(paste0("Class thresholds: ",
                      paste0(names(adjustments), " ",
                             format_threshold(adjustments),
                             collapse = ", ")))
    },
    if (nrow(skipped) > 0) {
      shiny::p(
        class = "text-warning-emphasis",
        shiny::icon("triangle-exclamation"),
        paste0(" Skipped threshold(s): ",
               paste0(skipped$class_name, " (", skipped$reason, ")",
                      collapse = "; "))
      )
    },
    if (length(replaced) > 0) {
      shiny::p(
        style = "color: #dc3545;",
        shiny::icon("triangle-exclamation"),
        paste0(" This will replace your ", paste(replaced, collapse = " and "),
               ".")
      )
    }
  )
}
