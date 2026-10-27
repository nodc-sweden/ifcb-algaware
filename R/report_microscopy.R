# Report text describing the microscopy stations (stations without IFCB data
# that were analysed by qualitative light microscopy instead).

#' One row per microscopy station visit
#' @keywords internal
microscopy_visits <- function(microscopy) {
  cols <- c("visit_id", "STATION_NAME_SHORT", "COAST", "visit_date",
            "sampler", "min_depth_m", "max_depth_m", "analysed_by")
  visits <- microscopy[!duplicated(microscopy$visit_id),
                       intersect(cols, names(microscopy)), drop = FALSE]
  visits[order(factor(visits$COAST, levels = c("WEST", "EAST")),
               visits$visit_date), , drop = FALSE]
}

#' Join words as "a", "a and b" or "a, b and c"
#' @keywords internal
join_words <- function(x) {
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

#' Introduction sentence for the microscopy stations
#'
#' @param microscopy Microscopy observations used in the report, or NULL.
#' @return A sentence (with a leading space) naming the stations, the
#'   sampling method and the analysts, or \code{""}.
#' @keywords internal
microscopy_intro_text <- function(microscopy) {
  if (is.null(microscopy) || nrow(microscopy) == 0) return("")
  visits <- microscopy_visits(microscopy)
  plural <- nrow(visits) > 1

  # "HOS (Hose)" -> "hose"; codes without a description are used as is.
  sampler <- ifelse(is.na(visits$sampler), "",
                    tolower(sub("^.*\\((.*)\\).*$", "\\1", visits$sampler)))
  depth <- ifelse(is.na(visits$min_depth_m) | is.na(visits$max_depth_m), "",
                  paste0(visits$min_depth_m, "\u2013", visits$max_depth_m, " m"))
  sample_kind <- unique(trimws(paste(depth, sampler)))
  sample_kind <- sample_kind[nzchar(sample_kind)]
  sample_text <- if (length(sample_kind) == 0) {
    if (plural) "samples" else "a sample"
  } else if (plural) {
    paste(join_words(sample_kind), "samples")
  } else {
    paste0(if (grepl("^[aeiou]", sample_kind)) "an " else "a ", sample_kind,
           " sample")
  }

  analysts <- unique(stats::na.omit(visits$analysed_by))
  analyst_text <- if (length(analysts) > 0) {
    paste0(" (analysed by ", join_words(analysts), ")")
  } else {
    ""
  }

  paste0(
    " No IFCB data were collected at ", join_words(visits$STATION_NAME_SHORT),
    if (plural) "; these stations are" else "; this station is",
    " described from qualitative light microscopy ",
    if (plural) "analyses of " else "analysis of ", sample_text, analyst_text,
    " and ", if (plural) "are" else "is", " not included in the figures."
  )
}

#' Table 1 value listing the microscopy samples
#' @keywords internal
microscopy_sample_label <- function(microscopy) {
  if (is.null(microscopy) || nrow(microscopy) == 0) return("")
  visits <- microscopy_visits(microscopy)
  paste0(visits$STATION_NAME_SHORT, " (", visits$visit_date, ")",
         collapse = ", ")
}
