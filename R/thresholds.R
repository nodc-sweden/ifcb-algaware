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
  threshold <- unname(adjustments[auto])
  affected <- follows_threshold_rule(classifications) & !is.na(threshold)
  if (!any(affected)) return(classifications)

  result <- classifications
  result$class_name[affected] <- ifelse(
    classifications$score[affected] >= threshold[affected],
    auto[affected],
    "unclassified"
  )
  result
}

#' Rows whose stored label came from the classifier's threshold rule
#'
#' @param classifications A data.frame with \code{class_name} and
#'   \code{class_auto} columns.
#' @return Logical vector: \code{TRUE} where the stored label is the
#'   top-scoring class or \code{"unclassified"}, so a threshold change may
#'   relabel the row. All \code{FALSE} without a \code{class_auto} column.
#' @keywords internal
follows_threshold_rule <- function(classifications) {
  auto <- classifications$class_auto
  stored <- classifications$class_name
  if (is.null(auto)) return(rep(FALSE, nrow(classifications)))
  !is.na(auto) & !is.na(stored) & (stored == auto | stored == "unclassified")
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
  idx <- correction_row_index(classifications, corrections)
  valid <- !is.na(idx)
  if (!any(valid)) return(classifications)

  result <- classifications
  result$class_name[idx[valid]] <- corrections$new_class[valid]
  result
}

#' Row of each correction in a classifications table
#'
#' Matches on numeric keys (sample index and ROI number) instead of pasted
#' text keys, which dominated the run time on a full cruise.
#'
#' @param classifications A data.frame with \code{sample_name} and
#'   \code{roi_number} columns.
#' @param corrections A data.frame with \code{sample_name} and
#'   \code{roi_number} columns, or \code{NULL}.
#' @return Integer vector with one element per correction: the row it
#'   applies to, or \code{NA} when the ROI is not in
#'   \code{classifications}. Empty when there are no corrections.
#' @keywords internal
correction_row_index <- function(classifications, corrections) {
  if (is.null(corrections) || nrow(corrections) == 0) return(integer(0))

  # ROI numbers stay far below the multiplier, so keys are unique and exact
  samples <- unique(corrections$sample_name)
  row_keys <- match(classifications$sample_name, samples) * 1e7 +
    classifications$roi_number
  correction_keys <- match(corrections$sample_name, samples) * 1e7 +
    corrections$roi_number

  idx <- match(correction_keys, row_keys)
  # A correction without a sample or ROI must not match rows with NA keys
  idx[is.na(correction_keys)] <- NA_integer_
  idx
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
#' Convenience wrapper around \code{preview_context()} and
#' \code{preview_from_context()}; the app keeps the context between slider
#' moves instead.
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
#'   \code{n_added} (would join the class from unclassified), plus the image
#'   IDs (\code{"<sample_name>_<roi_number>"}, as used by the gallery) of the
#'   moving images in \code{removed} and \code{added}.
#' @keywords internal
preview_threshold <- function(original, adjustments, corrections, class_name,
                              value, samples = NULL) {
  context <- preview_context(original, adjustments, corrections, class_name,
                             samples)
  preview_from_context(context, value)
}

#' Everything about a class's preview that does not depend on the slider
#'
#' Finding the rows a class can hold and applying the current thresholds and
#' corrections to them takes a noticeable fraction of a second on a full
#' cruise. None of it changes while the slider moves, so it is done once per
#' class and reused for every slider value.
#'
#' @inheritParams preview_threshold
#' @return A list with, per candidate row, \code{sample_name},
#'   \code{roi_number}, \code{score}, \code{was} (currently labelled with the
#'   class) and \code{follows} (the label follows this class's threshold:
#'   top class is the class, stored label came from the threshold rule, and
#'   no manual correction).
#' @keywords internal
preview_context <- function(original, adjustments, corrections, class_name,
                            samples = NULL) {
  candidates <- threshold_candidates(original, corrections, class_name)
  if (!is.null(samples)) {
    candidates <- candidates[candidates$sample_name %in% samples, ,
                             drop = FALSE]
  }
  current <- apply_thresholds(candidates, adjustments)$class_name
  follows <- follows_threshold_rule(candidates) &
    candidates$class_auto %in% class_name

  # A manual correction fixes the label whatever the threshold
  idx <- correction_row_index(candidates, corrections)
  corrected <- idx[!is.na(idx)]
  current[corrected] <- corrections$new_class[!is.na(idx)]
  follows[corrected] <- FALSE

  list(
    sample_name = candidates$sample_name,
    roi_number = candidates$roi_number,
    score = candidates$score,
    was = current %in% class_name,
    follows = follows
  )
}

#' Preview one threshold value from a prepared context
#'
#' @param context A list from \code{preview_context()}.
#' @param value Proposed threshold.
#' @return A list as returned by \code{preview_threshold()}.
#' @keywords internal
preview_from_context <- function(context, value) {
  was <- context$was
  now <- was
  reaches <- context$score[context$follows] >= value
  now[context$follows] <- !is.na(reaches) & reaches

  # Built for the moving images only; paste0() on empty input gives "_"
  image_ids <- function(rows) {
    if (!any(rows)) return(character(0))
    paste0(context$sample_name[rows], "_", context$roi_number[rows])
  }
  list(
    n_current = sum(was),
    n_removed = sum(was & !now),
    n_added = sum(!was & now),
    removed = image_ids(was & !now),
    added = image_ids(!was & now)
  )
}

#' Rows that can carry a class before or after a threshold change
#'
#' An image can only be labelled with a class if it is the image's
#' top-scoring class, its stored label, or the target of a manual
#' correction. All other rows are irrelevant to a preview of that class.
#' The correction match is deliberately loose (sample and ROI number matched
#' separately, avoiding a key for every row); extra rows are harmless.
#'
#' @param original The load-time classifications.
#' @param corrections Corrections log data.frame, or \code{NULL}.
#' @param class_name Class of interest.
#' @return The subset of \code{original} that may be labelled
#'   \code{class_name}, in the original row order.
#' @keywords internal
threshold_candidates <- function(original, corrections, class_name) {
  is_class <- function(x) !is.na(x) & x == class_name
  keep <- is_class(original$class_name)
  if (!is.null(original$class_auto)) {
    keep <- keep | is_class(original$class_auto)
  }
  if (!is.null(corrections) && nrow(corrections) > 0) {
    into <- corrections[corrections$new_class %in% class_name, , drop = FALSE]
    if (nrow(into) > 0) {
      keep <- keep | (original$sample_name %in% into$sample_name &
                        original$roi_number %in% into$roi_number)
    }
  }
  original[keep, , drop = FALSE]
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
