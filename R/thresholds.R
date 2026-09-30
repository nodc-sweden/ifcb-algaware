#' Apply adjusted class thresholds to classifications
#'
#' Re-evaluates the threshold rule used by ifcb-classify for the adjusted
#' classes only: an image whose top-scoring class (\code{class_auto}) is an
#' adjusted class gets that class when its score is at least the adjusted
#' threshold, otherwise \code{"unclassified"}. Images of classes without an
#' adjustment keep their stored \code{class_name}, so an empty
#' \code{adjustments} leaves the data untouched.
#'
#' Rows whose stored label is neither \code{class_auto} nor
#' \code{"unclassified"} did not come from the threshold rule and are never
#' changed, nor are rows without \code{class_auto}.
#'
#' @param classifications A data.frame from \code{read_h5_classifications()}
#'   with \code{class_name}, \code{class_auto} and \code{score} columns.
#' @param adjustments Named numeric vector (class name -> adjusted threshold),
#'   or \code{NULL}/empty for none.
#' @return A data.frame like \code{classifications} with updated
#'   \code{class_name}.
#' @keywords internal
apply_thresholds <- function(classifications, adjustments) {
  if (length(adjustments) == 0 ||
      !"class_auto" %in% names(classifications)) {
    return(classifications)
  }

  auto <- classifications$class_auto
  stored <- classifications$class_name
  rule_based <- !is.na(auto) & !is.na(stored) &
    (stored == auto | stored == "unclassified")
  threshold <- unname(adjustments[auto])
  affected <- rule_based & !is.na(threshold)
  if (!any(affected)) return(classifications)

  result <- classifications
  result$class_name[affected] <- ifelse(
    classifications$score[affected] >= threshold[affected],
    auto[affected],
    "unclassified"
  )
  result
}

#' Apply a corrections log to classifications
#'
#' Sets \code{class_name} to \code{new_class} for every ROI in the
#' corrections log. When an ROI appears more than once, the last row wins
#' (the log is chronological). Corrections for ROIs not in
#' \code{classifications} are ignored.
#'
#' @param classifications A data.frame with \code{sample_name},
#'   \code{roi_number} and \code{class_name} columns.
#' @param corrections A data.frame with \code{sample_name},
#'   \code{roi_number} and \code{new_class} columns, or \code{NULL}.
#' @return A data.frame like \code{classifications} with corrected
#'   \code{class_name}.
#' @keywords internal
apply_corrections <- function(classifications, corrections) {
  if (is.null(corrections) || nrow(corrections) == 0) {
    return(classifications)
  }

  keys <- paste0(classifications$sample_name, "_",
                 classifications$roi_number)
  correction_keys <- paste0(corrections$sample_name, "_",
                            corrections$roi_number)
  idx <- match(correction_keys, keys)
  valid <- !is.na(idx)
  if (!any(valid)) return(classifications)

  result <- classifications
  result$class_name[idx[valid]] <- corrections$new_class[valid]
  result
}

#' Build the working classifications from the load-time snapshot
#'
#' The single place where working labels are derived: the classifier output
#' as loaded, then threshold adjustments, then manual corrections (so a
#' manual decision always overrides a threshold), then the active-sample
#' filter.
#'
#' @param original The load-time classifications (all samples).
#' @param adjustments Named numeric vector of adjusted thresholds, or
#'   \code{NULL}.
#' @param corrections Corrections log data.frame, or \code{NULL}.
#' @param active_samples Optional character vector of sample names to keep
#'   in the active slice. \code{NULL} keeps all samples.
#' @return A list with \code{all} (every sample) and \code{active} (only
#'   \code{active_samples}).
#' @keywords internal
compose_classifications <- function(original, adjustments, corrections,
                                    active_samples = NULL) {
  all <- apply_corrections(apply_thresholds(original, adjustments),
                           corrections)
  active <- if (is.null(active_samples)) {
    all
  } else {
    all[all$sample_name %in% active_samples, , drop = FALSE]
  }
  list(all = all, active = active)
}

#' Set or reset the threshold adjustment of one class
#'
#' @param adjustments Named numeric vector of current adjustments (may be
#'   empty or \code{NULL}).
#' @param class_name Class to adjust. Must have a trained threshold.
#' @param value New threshold between 0 and 1, or \code{NULL} to reset the
#'   class to its trained threshold.
#' @param trained Named numeric vector of trained thresholds.
#' @param tolerance A \code{value} within this distance of the trained
#'   threshold counts as a reset.
#' @return The updated named numeric vector of adjustments. Classes back at
#'   their trained threshold are dropped, so the vector only ever holds real
#'   adjustments.
#' @keywords internal
set_threshold_adjustment <- function(adjustments, class_name, value, trained,
                                     tolerance = 1e-6) {
  if (!class_name %in% names(trained)) {
    stop("Class '", class_name, "' has no trained threshold.", call. = FALSE)
  }
  kept <- adjustments[names(adjustments) != class_name]
  if (is.null(kept)) kept <- numeric(0)
  if (is.null(value)) return(kept)

  if (!is.numeric(value) || length(value) != 1 || is.na(value) ||
      value < 0 || value > 1) {
    stop("Threshold must be a number between 0 and 1.", call. = FALSE)
  }
  if (abs(value - trained[[class_name]]) < tolerance) return(kept)

  replace_named(adjustments, class_name, value)
}

#' Replace or append one element of a named vector, keeping element order
#'
#' @param x Named vector (or \code{NULL}).
#' @param name Element name.
#' @param value New value.
#' @return The updated vector.
#' @keywords internal
replace_named <- function(x, name, value) {
  if (name %in% names(x)) {
    x[name] <- value
    return(x)
  }
  c(x, stats::setNames(value, name))
}

#' Preview the effect of a proposed threshold for one class
#'
#' Compares the working labels under the current adjustments with those
#' under the proposed threshold, both with manual corrections applied, so
#' manually corrected images are never counted.
#'
#' @param original The load-time classifications.
#' @param adjustments Current named numeric vector of adjustments.
#' @param corrections Corrections log data.frame, or \code{NULL}.
#' @param class_name Class whose threshold is being changed.
#' @param value Proposed threshold.
#' @param samples Optional character vector of samples to count within
#'   (e.g. the current region). \code{NULL} counts all samples.
#' @return A list with integer counts \code{n_current} (images currently in
#'   the class), \code{n_removed} (would move to unclassified) and
#'   \code{n_added} (would join the class from unclassified).
#' @keywords internal
preview_threshold <- function(original, adjustments, corrections, class_name,
                              value, samples = NULL) {
  if (!is.null(samples)) {
    original <- original[original$sample_name %in% samples, , drop = FALSE]
  }
  proposed <- replace_named(adjustments, class_name, value)
  before <- compose_classifications(original, adjustments, corrections)$all
  after <- compose_classifications(original, proposed, corrections)$all

  was <- before$class_name %in% class_name
  now <- after$class_name %in% class_name
  list(
    n_current = sum(was),
    n_removed = sum(was & !now),
    n_added = sum(!was & now)
  )
}

#' Summarise threshold adjustments for export and reporting
#'
#' @param trained Named numeric vector of trained thresholds.
#' @param adjustments Named numeric vector of adjusted thresholds.
#' @param original The load-time classifications.
#' @return A data.frame with \code{class_name}, \code{trained},
#'   \code{adjusted} and \code{n_moved} (images whose label the adjustment
#'   changes, before manual corrections), one row per adjusted class.
#' @keywords internal
threshold_summary <- function(trained, adjustments, original) {
  if (length(adjustments) == 0) {
    return(data.frame(class_name = character(0), trained = numeric(0),
                      adjusted = numeric(0), n_moved = integer(0),
                      stringsAsFactors = FALSE))
  }

  adjusted <- apply_thresholds(original, adjustments)
  changed <- adjusted$class_name != original$class_name
  changed[is.na(changed)] <- FALSE
  classes <- names(adjustments)
  n_moved <- vapply(classes, function(cls) {
    sum(changed & original$class_auto %in% cls)
  }, integer(1))

  data.frame(
    class_name = classes,
    trained = unname(trained[classes]),
    adjusted = unname(adjustments),
    n_moved = unname(n_moved),
    stringsAsFactors = FALSE
  )
}
