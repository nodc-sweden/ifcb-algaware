#' Fetch metadata from the IFCB Dashboard
#'
#' Wraps \code{iRfcb::ifcb_download_dashboard_metadata()} and extracts
#' available cruise numbers.
#'
#' @param dashboard_url Dashboard base URL.
#' @param dataset_name Dataset name (e.g. "RV_Svea").
#' @return A list with \code{metadata} (data.frame) and \code{cruise_numbers}
#'   (character vector, possibly empty if no cruise column exists).
#' @export
fetch_dashboard_metadata <- function(dashboard_url, dataset_name = NULL) {
  metadata <- iRfcb::ifcb_download_dashboard_metadata(
    dashboard_url,
    dataset_name = dataset_name,
    quiet = TRUE
  )

  cruise_numbers <- character(0)
  if ("cruise" %in% names(metadata)) {
    cruise_numbers <- unique(metadata$cruise)
    cruise_numbers <- cruise_numbers[!is.na(cruise_numbers) & nzchar(cruise_numbers)]
  }

  list(metadata = metadata, cruise_numbers = cruise_numbers)
}

#' Filter metadata by cruise number or date range
#'
#' @param metadata Dashboard metadata data.frame.
#' @param cruise Optional cruise number to filter on.
#' @param date_from Optional start date (Date or character yyyy-mm-dd).
#' @param date_to Optional end date.
#' @return Filtered metadata data.frame.
#' @export
filter_metadata <- function(metadata, cruise = NULL, date_from = NULL, date_to = NULL) {
  if (!is.null(cruise) && nzchar(cruise) && "cruise" %in% names(metadata)) {
    return(metadata[metadata$cruise == cruise, ])
  }

  if (!is.null(date_from) && !is.null(date_to)) {
    # Parse sample_time or timestamp column
    time_col <- if ("sample_time" %in% names(metadata)) "sample_time" else "timestamp"
    sample_dates <- as.Date(metadata[[time_col]])
    date_from <- as.Date(date_from)
    date_to <- as.Date(date_to)
    return(metadata[sample_dates >= date_from & sample_dates <= date_to, ])
  }

  metadata
}

#' Fetch image count metadata from the IFCB Dashboard
#'
#' Retrieves per-sample metadata including image counts and coordinates
#' from the dashboard's export_metadata API endpoint. This is a lightweight
#' call that does not download any raw data files.
#'
#' @param dashboard_url Dashboard base URL (e.g. "https://ifcb.example.com").
#' @param dataset_name Dataset name (e.g. "RV_Svea").
#' @param start_date Start date (Date or character yyyy-mm-dd).
#' @param end_date End date (Date or character yyyy-mm-dd).
#' @return A data.frame with columns: pid, sample_time, latitude, longitude,
#'   n_images, ml_analyzed. Returns empty data.frame on failure.
#' @export
fetch_image_counts <- function(dashboard_url, dataset_name,
                               start_date, end_date) {
  url <- paste0(
    sub("/$", "", dashboard_url),
    "/api/export_metadata/", dataset_name,
    "?start_date=", as.character(start_date),
    "&end_date=", as.character(end_date)
  )

  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    raw_text <- httr2::resp_body_string(resp)
    df <- utils::read.csv(textConnection(raw_text), stringsAsFactors = FALSE)

    # Keep only rows with valid coordinates
    df <- df[!is.na(df$latitude) & !is.na(df$longitude) &
               df$latitude != 0 & df$longitude != 0, ]

    df[, c("pid", "sample_time", "latitude", "longitude",
           "n_images", "ml_analyzed")]
  }, error = function(e) {
    warning("Failed to fetch image counts: ", e$message, call. = FALSE)
    data.frame(
      pid = character(0), sample_time = character(0),
      latitude = numeric(0), longitude = numeric(0),
      n_images = integer(0), ml_analyzed = numeric(0),
      stringsAsFactors = FALSE
    )
  })
}

#' Download raw IFCB files for selected bins
#'
#' Downloads .roi, .adc, and .hdr files to local storage. Skips files that
#' already exist.
#'
#' @param dashboard_url Dashboard base URL.
#' @param sample_ids Character vector of sample PIDs.
#' @param dest_dir Destination directory.
#' @param progress_callback Optional function(current, total, message) for
#'   progress updates.
#' @return Invisible NULL.
#' @export
download_raw_data <- function(dashboard_url, sample_ids, dest_dir,
                              progress_callback = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Check which samples already exist
  existing <- tools::file_path_sans_ext(
    list.files(dest_dir, pattern = "\\.roi$", recursive = TRUE)
  )
  needed <- setdiff(sample_ids, existing)

  if (length(needed) == 0) {
    if (!is.null(progress_callback)) {
      progress_callback(length(sample_ids), length(sample_ids),
                        "Raw data already downloaded")
    }
    return(invisible(NULL))
  }

  if (!is.null(progress_callback)) {
    progress_callback(0, length(needed), "Downloading raw data...")
  }

  iRfcb::ifcb_download_dashboard_data(
    dashboard_url = dashboard_url,
    samples = needed,
    file_types = c("roi", "adc", "hdr"),
    dest_dir = dest_dir,
    quiet = TRUE
  )

  if (!is.null(progress_callback)) {
    progress_callback(length(needed), length(needed), "Raw data downloaded")
  }

  invisible(NULL)
}

#' Download feature files for selected bins
#'
#' @param dashboard_url Dashboard base URL (must include dataset path for
#'   features).
#' @param sample_ids Character vector of sample PIDs.
#' @param dest_dir Destination directory.
#' @param progress_callback Optional progress callback.
#' @return Invisible NULL.
#' @export
download_features <- function(dashboard_url, sample_ids, dest_dir,
                              progress_callback = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  existing <- tools::file_path_sans_ext(
    list.files(dest_dir, pattern = "\\.csv$", recursive = TRUE)
  )
  needed <- setdiff(sample_ids, existing)

  if (length(needed) == 0) {
    if (!is.null(progress_callback)) {
      progress_callback(length(sample_ids), length(sample_ids),
                        "Features already downloaded")
    }
    return(invisible(NULL))
  }

  if (!is.null(progress_callback)) {
    progress_callback(0, length(needed), "Downloading features...")
  }

  iRfcb::ifcb_download_dashboard_data(
    dashboard_url = dashboard_url,
    samples = needed,
    file_types = "features",
    dest_dir = dest_dir,
    quiet = TRUE
  )

  if (!is.null(progress_callback)) {
    progress_callback(length(needed), length(needed), "Features downloaded")
  }

  invisible(NULL)
}

#' Copy classification H5 files for selected bins
#'
#' Locates \code{*_class.h5} files by constructing direct paths from sample
#' IDs rather than listing the full directory tree. The classification path
#' is expected to contain yearly subfolders (e.g. \code{class2024_v3}).
#' Only the subfolder matching each sample's year is searched.
#'
#' @param classification_path Source directory containing yearly subfolders
#'   with .h5 files.
#' @param sample_ids Character vector of sample PIDs (e.g.
#'   \code{"D20221023T000155_IFCB134"}).
#' @param dest_dir Destination directory.
#' @param progress_callback Optional progress callback.
#' @return Invisible character vector of copied file paths.
#' @export
copy_classification_files <- function(classification_path, sample_ids,
                                      dest_dir, progress_callback = NULL) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Skip samples already copied locally
  existing_h5 <- list.files(dest_dir, pattern = "\\.h5$")
  existing_samples <- sub("_class.*\\.h5$", "", existing_h5)
  needed <- setdiff(sample_ids, existing_samples)

  if (length(needed) == 0) {
    if (!is.null(progress_callback)) {
      progress_callback(length(sample_ids), length(sample_ids),
                        "Classification files already copied")
    }
    return(invisible(character(0)))
  }

  # Extract year from sample ID (format: D20221023T...)
  sample_years <- substr(needed, 2, 5)

  # Find yearly subfolders once (quick: only top-level dirs)
  subdirs <- list.dirs(classification_path, recursive = FALSE,
                       full.names = TRUE)
  subdir_names <- basename(subdirs)

  if (!is.null(progress_callback)) {
    progress_callback(0, length(needed),
                      paste0("Copying ", length(needed),
                             " classification files..."))
  }

  copied <- vapply(seq_along(needed), function(i) {
    sid <- needed[i]
    year <- sample_years[i]
    h5_name <- paste0(sid, "_class.h5")
    dest <- file.path(dest_dir, h5_name)

    # Find the yearly subfolder matching this sample's year
    year_dirs <- subdirs[grepl(paste0("class", year), subdir_names)]

    src <- NULL
    for (yd in year_dirs) {
      candidate <- file.path(yd, h5_name)
      if (file.exists(candidate)) {
        src <- candidate
        break
      }
    }

    if (!is.null(src)) {
      file.copy(src, dest, overwrite = FALSE)
      dest
    } else {
      NA_character_
    }
  }, character(1))
  copied <- copied[!is.na(copied)]

  if (!is.null(progress_callback)) {
    progress_callback(length(sample_ids), length(sample_ids),
                      "Classification files ready")
  }

  invisible(copied)
}

#' Read classifications from H5 files
#'
#' Reads thresholded class assignments from H5 classification files.
#'
#' @param h5_dir Directory containing .h5 files.
#' @param sample_ids Optional character vector of sample PIDs to read.
#'   If NULL, reads all .h5 files in the directory.
#' @return A data.frame with columns: sample_name, roi_number, class_name,
#'   score.
#' @export
read_h5_classifications <- function(h5_dir, sample_ids = NULL) {
  h5_files <- list.files(h5_dir, pattern = "_class.*\\.h5$",
                         full.names = TRUE, recursive = TRUE)

  if (!is.null(sample_ids)) {
    h5_samples <- sub("_class.*\\.h5$", "", basename(h5_files))
    h5_files <- h5_files[h5_samples %in% sample_ids]
  }

  if (length(h5_files) == 0) {
    return(data.frame(
      sample_name = character(0),
      roi_number = integer(0),
      class_name = character(0),
      score = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- lapply(h5_files, function(h5_path) {
    tryCatch({
      h5 <- hdf5r::H5File$new(h5_path, "r")
      on.exit(h5$close_all(), add = TRUE)

      roi_numbers <- h5[["roi_numbers"]]$read()
      class_names <- h5[["class_name"]]$read()
      output_scores <- h5[["output_scores"]]$read()
      scores <- apply(output_scores, 2, max)

      sample_name <- sub("_class.*\\.h5$", "", basename(h5_path))

      data.frame(
        sample_name = sample_name,
        roi_number = as.integer(roi_numbers),
        class_name = class_names,
        score = scores,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("Failed to read H5 file: ", basename(h5_path), " - ", e$message,
              call. = FALSE)
      NULL
    })
  })

  valid_results <- Filter(Negate(is.null), results)
  if (length(valid_results) == 0) {
    return(data.frame(sample_name = character(0), roi_number = integer(0),
                      class_name = character(0), score = numeric(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, valid_results)
}

#' Read classifier name from an H5 classification file
#'
#' Extracts the \code{classifier_name} attribute from the first available
#' H5 file in a directory.
#'
#' @param h5_dir Directory containing .h5 files.
#' @return Character string with the classifier name, or NULL if unavailable.
#' @export
read_classifier_name <- function(h5_dir) {
  h5_files <- list.files(h5_dir, pattern = "_class.*\\.h5$",
                         full.names = TRUE, recursive = TRUE)
  if (length(h5_files) == 0) return(NULL)

  tryCatch({
    h5 <- hdf5r::H5File$new(h5_files[1], "r")
    on.exit(h5$close_all(), add = TRUE)
    if (h5$exists("classifier_name")) {
      h5[["classifier_name"]]$read()
    } else {
      NULL
    }
  }, error = function(e) NULL)
}
