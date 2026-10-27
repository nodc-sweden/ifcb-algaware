# Microscopy gap-fill: qualitative Plankton Toolbox samples for AlgAware
# stations that the IFCB did not sample during a cruise. The observations
# only feed the report text (station descriptions and summaries); they are
# kept apart from station_summary so no biovolume figure or calculation ever
# sees them.

#' Plankton Toolbox qualitative abundance scale
#'
#' Labels for the 1-5 abundance classes used in qualitative Plankton Toolbox
#' counts.
#' @keywords internal
ptbx_abundance_labels <- c(
  "1" = "Observed",
  "2" = "Several cells",
  "3" = "1-10%",
  "4" = "10-50%",
  "5" = "50-100%"
)

#' Read a qualitative Plankton Toolbox sample
#'
#' Reads one Plankton Toolbox Excel export with
#' \code{SHARK4R::read_ptbx()} and returns one row per observed taxon with
#' the sample metadata repeated on every row.
#'
#' @param path Path to the Plankton Toolbox \code{.xlsx} export.
#' @param file_name Name to report the sample under (defaults to the file
#'   name; Shiny uploads use the original name of the uploaded file).
#' @return A data.frame, see \code{parse_ptbx_sample()}.
#' @export
read_microscopy_sample <- function(path, file_name = basename(path)) {
  info <- SHARK4R::read_ptbx(path, sheet = "sample_info.txt")
  data <- SHARK4R::read_ptbx(path, sheet = "sample_data.txt")
  method <- SHARK4R::read_ptbx(path, sheet = "counting_method.txt")
  parse_ptbx_sample(info, data, method, file_name)
}

#' Parse the sheets of a Plankton Toolbox export
#'
#' @param info The \code{sample_info.txt} sheet (\code{key}/\code{value}).
#' @param data The \code{sample_data.txt} sheet.
#' @param method The \code{counting_method.txt} sheet.
#' @param file_name Name of the source file.
#' @return A data.frame with one row per taxon, sorted by abundance class
#'   (highest first), with columns \code{sample_file},
#'   \code{station_reported}, \code{sample_date}, \code{latitude},
#'   \code{longitude}, \code{min_depth_m}, \code{max_depth_m},
#'   \code{sampler}, \code{counting_method}, \code{analysed_by},
#'   \code{name}, \code{display_name}, \code{taxon_class},
#'   \code{trophic_type}, \code{abundance_class} and
#'   \code{abundance_label}.
#' @keywords internal
parse_ptbx_sample <- function(info, data, method, file_name) {
  values <- stats::setNames(as.character(info$value), as.character(info$key))
  field <- function(key) {
    value <- trimws(unname(values[key]))
    if (length(value) == 0 || is.na(value) || !nzchar(value)) {
      NA_character_
    } else {
      value
    }
  }

  station <- field("station_name")
  if (is.na(station)) stop("the sample has no station name", call. = FALSE)
  sample_date <- tryCatch(as.Date(field("sample_date")),
                          error = function(e) as.Date(NA))
  if (is.na(sample_date)) {
    stop("the sample has no valid sample date", call. = FALSE)
  }

  qualitative <- if ("qualitative_quantitative" %in% names(method)) {
    trimws(as.character(method$qualitative_quantitative[1]))
  } else {
    NA_character_
  }
  if (!identical(tolower(qualitative), "qualitative")) {
    stop("only qualitative samples are supported (this one is ",
         if (is.na(qualitative)) "unspecified" else qualitative, ")",
         call. = FALSE)
  }

  missing_cols <- setdiff(c("scientific_name", "abundance_class"), names(data))
  if (length(missing_cols) > 0) {
    stop("sample_data.txt lacks ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  data <- data[!is.na(data$scientific_name) &
                 nzchar(trimws(data$scientific_name)), , drop = FALSE]
  if (nrow(data) == 0) stop("the sample has no taxa", call. = FALSE)

  abundance <- suppressWarnings(as.integer(data$abundance_class))
  if (any(is.na(abundance) | !abundance %in% 1:5)) {
    stop("every taxon needs an abundance class from 1 to 5", call. = FALSE)
  }

  text_col <- function(col) {
    if (col %in% names(data)) as.character(data[[col]]) else NA_character_
  }
  name <- trimws(as.character(data$scientific_name))
  full_name <- trimws(text_col("scientific_full_name"))
  display_name <- ifelse(is.na(full_name) | !nzchar(full_name), name,
                         full_name)

  taxa <- data.frame(
    name = name,
    display_name = display_name,
    taxon_class = text_col("taxon_class"),
    trophic_type = text_col("trophic_type"),
    abundance_class = abundance,
    stringsAsFactors = FALSE
  )
  # A taxon can appear on several rows (e.g. different method steps); keep
  # its highest class.
  taxa <- taxa[order(-taxa$abundance_class, taxa$display_name), ]
  taxa <- taxa[!duplicated(taxa$display_name), ]
  taxa$abundance_label <- unname(
    ptbx_abundance_labels[as.character(taxa$abundance_class)]
  )

  numeric_field <- function(key) suppressWarnings(as.numeric(field(key)))
  method_field <- function(col) {
    if (col %in% names(method)) trimws(as.character(method[[col]][1])) else NA
  }
  meta <- data.frame(
    sample_file = file_name,
    station_reported = station,
    sample_date = sample_date,
    latitude = numeric_field("sample_latitude_dd"),
    longitude = numeric_field("sample_longitude_dd"),
    min_depth_m = numeric_field("sample_min_depth_m"),
    max_depth_m = numeric_field("sample_max_depth_m"),
    sampler = field("sampler_type_code"),
    counting_method = method_field("counting_method"),
    analysed_by = field("analysed_by"),
    stringsAsFactors = FALSE
  )
  out <- cbind(meta[rep(1L, nrow(taxa)), , drop = FALSE], taxa)
  rownames(out) <- NULL
  out
}

#' Match a microscopy sample to an AlgAware station
#'
#' Tries, in order: the station name (case-insensitive, against both the
#' full and short AlgAware names), the synonyms in the SHARK station
#' register, and the sample position within the station's
#' \code{OUT_OF_BOUNDS_RADIUS} (the same rule used for IFCB bins).
#'
#' @param station_reported Station name written in the sample.
#' @param latitude,longitude Sample position in decimal degrees, or NA.
#' @param algaware_stations Data frame from \code{load_algaware_stations()}.
#' @param shark_stations SHARK station register from
#'   \code{load_shark_stations()}.
#' @return A one-row data.frame with \code{STATION_NAME}, \code{COAST},
#'   \code{STATION_NAME_SHORT} and \code{match_method} (all NA when no
#'   station matches).
#' @keywords internal
match_microscopy_station <- function(station_reported, latitude, longitude,
                                     algaware_stations, shark_stations) {
  normalise <- function(x) {
    toupper(trimws(gsub("\\s+", " ", enc2utf8(as.character(x)))))
  }
  algaware_stations <- as_utf8_columns(algaware_stations)
  result <- function(i, method) {
    if (is.na(i)) {
      return(data.frame(STATION_NAME = NA_character_, COAST = NA_character_,
                        STATION_NAME_SHORT = NA_character_,
                        match_method = NA_character_,
                        stringsAsFactors = FALSE))
    }
    data.frame(algaware_stations[i, c("STATION_NAME", "COAST",
                                      "STATION_NAME_SHORT")],
               match_method = method, stringsAsFactors = FALSE,
               row.names = NULL)
  }

  reported <- normalise(station_reported)
  i <- match(reported, normalise(algaware_stations$STATION_NAME))
  if (is.na(i)) i <- match(reported, normalise(algaware_stations$STATION_NAME_SHORT))
  if (!is.na(i)) return(result(i, "name"))

  shark <- shark_stations
  if (is.null(shark) || nrow(shark) == 0) return(result(NA, NA))
  shark$STATION_NAME <- enc2utf8(shark$STATION_NAME)
  shark <- shark[shark$STATION_NAME %in% algaware_stations$STATION_NAME, ,
                 drop = FALSE]

  if ("SYNONYM_NAMES" %in% names(shark)) {
    for (k in seq_len(nrow(shark))) {
      synonyms <- strsplit(enc2utf8(shark$SYNONYM_NAMES[k]), "<or>",
                           fixed = TRUE)[[1]]
      if (reported %in% normalise(synonyms)) {
        return(result(match(shark$STATION_NAME[k],
                            algaware_stations$STATION_NAME), "synonym"))
      }
    }
  }

  if (is.na(latitude) || is.na(longitude) || nrow(shark) == 0) {
    return(result(NA, NA))
  }
  dist_m <- haversine_m(latitude, longitude,
                        shark$LATITUDE_WGS84_SWEREF99_DD,
                        shark$LONGITUDE_WGS84_SWEREF99_DD)
  inside <- which(!is.na(dist_m) & dist_m <= shark$OUT_OF_BOUNDS_RADIUS)
  if (length(inside) == 0) return(result(NA, NA))
  nearest <- inside[which.min(dist_m[inside])]
  result(match(shark$STATION_NAME[nearest], algaware_stations$STATION_NAME),
         "position")
}

#' Great-circle distance in metres
#' @keywords internal
haversine_m <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(a)))
}

#' Read and station-match a set of microscopy samples
#'
#' A file that cannot be read, or whose station is not an AlgAware station,
#' is reported in \code{status} instead of stopping the whole load.
#'
#' @param paths Paths to Plankton Toolbox \code{.xlsx} exports.
#' @param file_names Names to report the files under.
#' @param algaware_stations Data frame from \code{load_algaware_stations()}.
#' @param shark_stations SHARK station register.
#' @return A list with \code{obs} (one row per taxon and sample, with
#'   \code{STATION_NAME}, \code{STATION_NAME_SHORT}, \code{COAST},
#'   \code{visit_date}, \code{visit_id} and \code{sample_index}) and
#'   \code{status} (one row per file: \code{sample_index}, \code{file},
#'   \code{station}, \code{date}, \code{used}, \code{reason}).
#' @export
load_microscopy_samples <- function(paths, file_names = basename(paths),
                                    algaware_stations = load_algaware_stations(),
                                    shark_stations = load_shark_stations(verbose = FALSE)) {
  results <- lapply(seq_along(paths), function(i) {
    status <- data.frame(sample_index = i, file = file_names[i],
                         station = NA_character_, date = as.Date(NA),
                         used = FALSE, reason = "", stringsAsFactors = FALSE)
    obs <- tryCatch(read_microscopy_sample(paths[i], file_names[i]),
                    error = function(e) e)
    if (inherits(obs, "error")) {
      status$reason <- paste("could not be read:", conditionMessage(obs))
      return(list(obs = NULL, status = status))
    }
    status$station <- obs$station_reported[1]
    status$date <- obs$sample_date[1]

    station <- match_microscopy_station(obs$station_reported[1],
                                        obs$latitude[1], obs$longitude[1],
                                        algaware_stations, shark_stations)
    if (is.na(station$STATION_NAME)) {
      status$reason <- paste0("station '", obs$station_reported[1],
                              "' is not an AlgAware station")
      return(list(obs = NULL, status = status))
    }

    obs$STATION_NAME <- station$STATION_NAME
    obs$STATION_NAME_SHORT <- station$STATION_NAME_SHORT
    obs$COAST <- station$COAST
    obs$visit_date <- obs$sample_date
    obs$visit_id <- paste0(station$STATION_NAME_SHORT, "_", obs$sample_date,
                           "_microscopy")
    obs$sample_index <- i
    status$station <- station$STATION_NAME_SHORT
    status$used <- TRUE
    list(obs = obs, status = status)
  })

  obs <- do.call(rbind, lapply(results, `[[`, "obs"))
  status <- do.call(rbind, lapply(results, `[[`, "status"))
  if (is.null(status)) {
    status <- data.frame(sample_index = integer(0), file = character(0),
                         station = character(0), date = as.Date(character(0)),
                         used = logical(0), reason = character(0),
                         stringsAsFactors = FALSE)
  }
  if (is.null(obs)) obs <- data.frame()
  list(obs = obs, status = status)
}

#' Keep the microscopy samples that fill a gap in the IFCB data
#'
#' IFCB data takes precedence: a sample is skipped when the loaded cruise
#' has IFCB data for its station. Samples dated more than
#' \code{window_days} outside the IFCB cruise dates (likely from another
#' cruise) and second samples for the same station are skipped too.
#'
#' @param loaded Result of \code{load_microscopy_samples()}.
#' @param station_summary IFCB station summary of the loaded cruise, or
#'   NULL to skip the IFCB checks.
#' @param window_days Allowed distance in days from the IFCB cruise dates.
#' @return \code{loaded} with \code{obs} restricted to the used samples and
#'   \code{status} updated with the reasons for skipping.
#' @export
select_gap_samples <- function(loaded, station_summary, window_days = 7) {
  status <- loaded$status
  has_ifcb <- !is.null(station_summary) && nrow(station_summary) > 0
  ifcb_stations <- if (has_ifcb) unique(station_summary$STATION_NAME) else NULL
  ifcb_dates <- if (has_ifcb) {
    range(as.Date(station_summary$visit_date), na.rm = TRUE)
  } else {
    NULL
  }

  used_stations <- character(0)
  for (k in which(status$used)) {
    rows <- loaded$obs[loaded$obs$sample_index == status$sample_index[k], ]
    station <- rows$STATION_NAME[1]
    sample_date <- rows$sample_date[1]

    reason <- if (has_ifcb && station %in% ifcb_stations) {
      "IFCB data exists for this station"
    } else if (has_ifcb &&
               (sample_date < ifcb_dates[1] - window_days ||
                  sample_date > ifcb_dates[2] + window_days)) {
      paste0("sample date is outside the cruise dates (",
             ifcb_dates[1], " to ", ifcb_dates[2], ")")
    } else if (station %in% used_stations) {
      "another microscopy sample for this station is already used"
    } else {
      ""
    }

    if (nzchar(reason)) {
      status$used[k] <- FALSE
      status$reason[k] <- reason
    } else {
      used_stations <- c(used_stations, station)
    }
  }

  obs <- loaded$obs
  if (nrow(obs) > 0) {
    obs <- obs[obs$sample_index %in% status$sample_index[status$used], ,
               drop = FALSE]
  }
  list(obs = obs, status = status)
}
