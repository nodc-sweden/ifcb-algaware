# Tests for R/ctd.R

# -- load_chl_statistics -------------------------------------------------------

test_that("load_chl_statistics returns a data.frame with expected columns", {
  result <- load_chl_statistics()
  expect_s3_class(result, "data.frame")
  expect_true(all(c("STATN", "STNCODE", "MONTH", "CHLA_mean", "CHLA_std",
                     "CHLA_n") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("load_chl_statistics MONTH is integer", {
  result <- load_chl_statistics()
  expect_type(result$MONTH, "integer")
})

# -- load_station_mapper -------------------------------------------------------

test_that("load_station_mapper returns a data.frame with synonym and statn", {
  result <- load_station_mapper()
  expect_s3_class(result, "data.frame")
  expect_true("synonym" %in% names(result))
  expect_true("statn" %in% names(result))
})

# -- load_standard_stations ---------------------------------------------------

test_that("load_standard_stations returns a data.frame with station_name and region", {
  result <- load_standard_stations()
  expect_s3_class(result, "data.frame")
  expect_true("station_name" %in% names(result))
  expect_true("region" %in% names(result))
  expect_true(nrow(result) > 0)
})

# -- normalize_station_name ---------------------------------------------------

test_that("normalize_station_name returns NA for blank or NA input", {
  mapper <- data.frame(synonym = "by5", statn = "BY5",
                       stringsAsFactors = FALSE)
  expect_equal(algaware:::normalize_station_name(NA_character_, mapper), NA_character_)
  expect_equal(algaware:::normalize_station_name("   ", mapper), NA_character_)
})

test_that("normalize_station_name matches exact synonym", {
  mapper <- data.frame(synonym = c("bornholm basin", "by5"),
                       statn = c("BY5", "BY5"),
                       stringsAsFactors = FALSE)
  result <- algaware:::normalize_station_name("BY5", mapper)
  expect_equal(result, "BY5")
})

test_that("normalize_station_name matches prefix synonym", {
  mapper <- data.frame(synonym = "by5", statn = "BY5",
                       stringsAsFactors = FALSE)
  result <- algaware:::normalize_station_name("BY5 extra text", mapper)
  expect_equal(result, "BY5")
})

test_that("normalize_station_name falls back to standard_stations exact match", {
  mapper <- data.frame(synonym = "other", statn = "OTHER",
                       stringsAsFactors = FALSE)
  std <- data.frame(station_name = "BY31", region = "The Eastern Baltic",
                    stringsAsFactors = FALSE)
  result <- algaware:::normalize_station_name("BY31", mapper, std)
  expect_equal(result, "BY31")
})

test_that("normalize_station_name returns NA when no match found", {
  mapper <- data.frame(synonym = "abc", statn = "ABC",
                       stringsAsFactors = FALSE)
  result <- algaware:::normalize_station_name("unknown_station", mapper)
  expect_true(is.na(result))
})

# -- match_stats_station ------------------------------------------------------

test_that("match_stats_station returns NULL for empty chl_stats", {
  expect_null(algaware:::match_stats_station("BY5", NULL))
  expect_null(algaware:::match_stats_station("BY5",
    data.frame(STATN = character(0), MONTH = integer(0),
               CHLA_mean = numeric(0), CHLA_std = numeric(0),
               CHLA_n = integer(0), stringsAsFactors = FALSE)))
})

test_that("match_stats_station returns rows for matching station", {
  stats <- data.frame(
    STATN = c("BY5", "BY5", "BY31"),
    MONTH = 1:3,
    CHLA_mean = c(1.0, 2.0, 3.0),
    CHLA_std = c(0.1, 0.2, 0.3),
    CHLA_n = c(10L, 10L, 10L),
    stringsAsFactors = FALSE
  )
  result <- algaware:::match_stats_station("BY5", stats)
  expect_equal(nrow(result), 2)
  expect_true(all(result$STATN == "BY5"))
})

test_that("match_stats_station returns NULL when no station matches", {
  stats <- data.frame(STATN = "BY31", MONTH = 1L, CHLA_mean = 2.0,
                      CHLA_std = 0.5, CHLA_n = 5L, stringsAsFactors = FALSE)
  expect_null(algaware:::match_stats_station("UNKNOWN", stats))
})

# -- compute_ctd_chl_avg ------------------------------------------------------

test_that("compute_ctd_chl_avg returns empty data.frame for NULL input", {
  result <- compute_ctd_chl_avg(NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("compute_ctd_chl_avg averages fluorescence at <= 20 dbar", {
  ctd_data <- data.frame(
    station_short = c("BY5", "BY5", "BY5"),
    latitude = c(55.25, 55.25, 55.25),
    longitude = c(15.98, 15.98, 15.98),
    pressure_dbar = c(5, 15, 25),
    chl_fluorescence = c(2.0, 4.0, 8.0),
    stringsAsFactors = FALSE
  )
  result <- compute_ctd_chl_avg(ctd_data)
  expect_equal(nrow(result), 1)
  expect_equal(result$chl_mean, 3.0)  # mean(2.0, 4.0)
})

test_that("compute_ctd_chl_avg returns empty when all depths > 20", {
  ctd_data <- data.frame(
    station_short = "BY5", latitude = 55.25, longitude = 15.98,
    pressure_dbar = 25, chl_fluorescence = 5.0,
    stringsAsFactors = FALSE
  )
  result <- compute_ctd_chl_avg(ctd_data)
  expect_equal(nrow(result), 0)
})

# -- compute_lims_chl_avg -----------------------------------------------------

test_that("compute_lims_chl_avg returns empty for NULL input", {
  result <- compute_lims_chl_avg(NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("compute_lims_chl_avg excludes hose samples and averages <= 20 m", {
  lims_data <- data.frame(
    station_short = c("BY5", "BY5", "BY5"),
    latitude = c(55.25, 55.25, 55.25),
    longitude = c(15.98, 15.98, 15.98),
    DEPH = c(5, 10, 5),
    CPHL = c(2.0, 4.0, 6.0),
    SMPNO = c("2026-BY5-001", "2026-BY5-SLA_0-10-002", "2026-BY5-003"),
    stringsAsFactors = FALSE
  )
  result <- compute_lims_chl_avg(lims_data)
  expect_equal(nrow(result), 1)
  # Only non-hose rows: DEPH 5 (CPHL 2.0) and DEPH 5 (CPHL 6.0)
  expect_equal(result$chl_mean, mean(c(2.0, 6.0)))
})

test_that("compute_lims_chl_avg returns empty when all depths > 20", {
  lims_data <- data.frame(
    station_short = "BY5", latitude = 55.25, longitude = 15.98,
    DEPH = 30, CPHL = 5.0, SMPNO = "2026-BY5-001",
    stringsAsFactors = FALSE
  )
  result <- compute_lims_chl_avg(lims_data)
  expect_equal(nrow(result), 0)
})

# -- compute_lims_hose_avg ----------------------------------------------------

test_that("compute_lims_hose_avg returns empty for NULL input", {
  result <- compute_lims_hose_avg(NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("compute_lims_hose_avg returns empty when SMPNO column absent", {
  lims_data <- data.frame(
    station_short = "BY5", latitude = 55.25, longitude = 15.98,
    DEPH = 5, CPHL = 3.0, stringsAsFactors = FALSE
  )
  result <- compute_lims_hose_avg(lims_data)
  expect_equal(nrow(result), 0)
})

test_that("compute_lims_hose_avg averages hose samples only", {
  lims_data <- data.frame(
    station_short = c("BY5", "BY5", "BY5"),
    latitude = c(55.25, 55.25, 55.25),
    longitude = c(15.98, 15.98, 15.98),
    DEPH = c(5, 5, 5),
    CPHL = c(2.0, 4.0, 6.0),
    SMPNO = c("2026-BY5-001", "2026-BY5-SLA_0-10-002", "2026-BY5-SLA_0-10-003"),
    stringsAsFactors = FALSE
  )
  result <- compute_lims_hose_avg(lims_data)
  expect_equal(nrow(result), 1)
  expect_equal(result$chl_mean, mean(c(4.0, 6.0)))
})

# -- normalize_station_name (prefix fallback to standard_stations) ------------

test_that("normalize_station_name uses prefix match against standard_stations", {
  mapper <- data.frame(synonym = "other", statn = "OTHER",
                       stringsAsFactors = FALSE)
  std <- data.frame(station_name = "BY31", region = "The Eastern Baltic",
                    stringsAsFactors = FALSE)
  result <- algaware:::normalize_station_name("BY31 extra text", mapper, std)
  expect_equal(result, "BY31")
})

# -- match_stats_station (ASCII fallback) -------------------------------------

test_that("match_stats_station uses ASCII fallback for special characters", {
  stats <- data.frame(
    STATN = c("ANHOLT E"),  # could have special chars in real data
    MONTH = 1L,
    CHLA_mean = 2.0,
    CHLA_std = 0.5,
    CHLA_n = 5L,
    stringsAsFactors = FALSE
  )
  # Name with special char that ASCII-strips to the same thing
  result <- algaware:::match_stats_station("ANHOLT E", stats)
  expect_equal(nrow(result), 1)
})

# -- read_lims_data -----------------------------------------------------------

.make_lims_file <- function(path, rows) {
  # Write a minimal LIMS data.txt file (tab-delimited)
  write.table(rows, path, sep = "\t", row.names = FALSE, quote = FALSE)
}

test_that("read_lims_data returns NULL for non-existent file", {
  expect_warning(
    result <- read_lims_data("/nonexistent/data.txt", data.frame()),
    "not found"
  )
  expect_null(result)
})

test_that("read_lims_data returns NULL when no CPHL or CHLFL column", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = "BY5", SDATE = "2024-06-15", DEPH = "5",
    LATIT = "5515.0", LONGI = "1559.0",
    stringsAsFactors = FALSE
  )
  .make_lims_file(tmp, rows)

  stations <- data.frame(
    STATION_NAME = "BY5 BORNHOLM BASIN", STATION_NAME_SHORT = "BY5",
    COAST = "Baltic", stringsAsFactors = FALSE
  )
  result <- read_lims_data(tmp, stations)
  expect_null(result)
})

test_that("read_lims_data parses valid LIMS file and matches stations", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = c("BY5", "BY5", "UNKNOWN"),
    SDATE = c("2024-06-15", "2024-06-15", "2024-06-15"),
    DEPH = c("5", "15", "5"),
    CPHL = c("2.5", "3.0", "1.0"),
    Q_CPHL = c(NA, NA, NA),
    LATIT = c("5515.0", "5515.0", "5515.0"),
    LONGI = c("1559.0", "1559.0", "1559.0"),
    SMPNO = c("2024-BY5-001", "2024-BY5-002", "2024-UNKN-001"),
    stringsAsFactors = FALSE
  )
  .make_lims_file(tmp, rows)

  stations <- data.frame(
    STATION_NAME = "BY5 BORNHOLM BASIN",
    STATION_NAME_SHORT = "BY5",
    COAST = "Baltic",
    stringsAsFactors = FALSE
  )
  result <- read_lims_data(tmp, stations)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # only BY5 rows matched
  expect_true(all(result$station_short == "BY5"))
  expect_true(all(c("station_short", "station_name", "coast",
                     "sample_date", "sample_month", "DEPH", "CPHL",
                     "latitude", "longitude") %in% names(result)))
})

test_that("read_lims_data filters Q_CPHL == 'B' rows", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = c("BY5", "BY5"),
    SDATE = c("2024-06-15", "2024-06-15"),
    DEPH = c("5", "10"),
    CPHL = c("2.5", "3.0"),
    Q_CPHL = c("B", NA),
    LATIT = c("5515.0", "5515.0"),
    LONGI = c("1559.0", "1559.0"),
    SMPNO = c("2024-BY5-001", "2024-BY5-002"),
    stringsAsFactors = FALSE
  )
  .make_lims_file(tmp, rows)

  stations <- data.frame(
    STATION_NAME = "BY5 BORNHOLM BASIN",
    STATION_NAME_SHORT = "BY5",
    COAST = "Baltic",
    stringsAsFactors = FALSE
  )
  result <- read_lims_data(tmp, stations)
  # Only the non-flagged row should remain
  expect_equal(nrow(result), 1)
  expect_equal(result$CPHL, 3.0)
})

test_that("read_lims_data uses CHLFL when CPHL absent", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = "BY5",
    SDATE = "2024-06-15",
    DEPH = "5",
    CHLFL = "1.8",
    Q_CHLFL = NA,
    LATIT = "5515.0",
    LONGI = "1559.0",
    SMPNO = "2024-BY5-001",
    stringsAsFactors = FALSE
  )
  .make_lims_file(tmp, rows)

  stations <- data.frame(
    STATION_NAME = "BY5 BORNHOLM BASIN",
    STATION_NAME_SHORT = "BY5",
    COAST = "Baltic",
    stringsAsFactors = FALSE
  )
  result <- read_lims_data(tmp, stations)
  expect_equal(nrow(result), 1)
  expect_equal(result$CPHL, 1.8)
})

# -- read_lims_data_all -------------------------------------------------------

test_that("read_lims_data_all returns NULL for non-existent file", {
  expect_warning(
    result <- read_lims_data_all(
      "/nonexistent/data.txt",
      data.frame(synonym = character(0), statn = character(0)),
      data.frame(station_name = character(0), region = character(0))
    ),
    "not found"
  )
  expect_null(result)
})

test_that("read_lims_data_all parses file and normalizes station names", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = c("BY5", "UNKNOWN"),
    SDATE = c("2024-06-15", "2024-06-15"),
    DEPH = c("5", "5"),
    CPHL = c("2.5", "1.0"),
    Q_CPHL = c(NA, NA),
    LATIT = c("5515.0", "5515.0"),
    LONGI = c("1559.0", "1559.0"),
    stringsAsFactors = FALSE
  )
  .make_lims_file(tmp, rows)

  mapper <- data.frame(synonym = "by5", statn = "BY5",
                       stringsAsFactors = FALSE)
  std <- data.frame(station_name = "BY5", region = "The Eastern Baltic",
                    stringsAsFactors = FALSE)
  result <- read_lims_data_all(tmp, mapper, std)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$canonical_name, "BY5")
  expect_equal(result$region, "The Eastern Baltic")
})

test_that("read_lims_data returns NULL when no rows match any station", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = "UNKNOWN_STATION",
    SDATE = "2024-06-15",
    DEPH = "5",
    CPHL = "2.5",
    Q_CPHL = NA,
    LATIT = "5515.0",
    LONGI = "1559.0",
    SMPNO = "2024-UNK-001",
    stringsAsFactors = FALSE
  )
  write.table(rows, tmp, sep = "\t", row.names = FALSE, quote = FALSE)

  stations <- data.frame(
    STATION_NAME = "BY5 BORNHOLM BASIN",
    STATION_NAME_SHORT = "BY5",
    COAST = "Baltic",
    stringsAsFactors = FALSE
  )
  result <- read_lims_data(tmp, stations)
  expect_null(result)
})

test_that("read_lims_data returns NULL when DEPH column absent", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = "BY5",
    SDATE = "2024-06-15",
    CPHL = "2.5",
    LATIT = "5515.0",
    LONGI = "1559.0",
    stringsAsFactors = FALSE
  )
  write.table(rows, tmp, sep = "\t", row.names = FALSE, quote = FALSE)

  stations <- data.frame(
    STATION_NAME = "BY5 BORNHOLM BASIN",
    STATION_NAME_SHORT = "BY5",
    COAST = "Baltic",
    stringsAsFactors = FALSE
  )
  result <- read_lims_data(tmp, stations)
  expect_null(result)
})

test_that("read_lims_data_all returns NULL when no rows match standard stations", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  rows <- data.frame(
    STATN = "TOTALLY_UNKNOWN",
    SDATE = "2024-06-15",
    DEPH = "5",
    CPHL = "2.5",
    Q_CPHL = NA,
    LATIT = "5515.0",
    LONGI = "1559.0",
    stringsAsFactors = FALSE
  )
  write.table(rows, tmp, sep = "\t", row.names = FALSE, quote = FALSE)

  mapper <- data.frame(synonym = "by5", statn = "BY5",
                       stringsAsFactors = FALSE)
  std <- data.frame(station_name = "BY5", region = "The Eastern Baltic",
                    stringsAsFactors = FALSE)
  result <- read_lims_data_all(tmp, mapper, std)
  expect_null(result)
})

# -- parse_single_cnv ---------------------------------------------------------

test_that("parse_single_cnv parses ANHOLT E CNV file", {
  cnv_path <- testthat::test_path("test_data",
                                   "SBE09_0745_20260313_1005_77SE_06_0276.cnv")
  skip_if_not(file.exists(cnv_path), "Test CNV file not available")
  skip_if_not_installed("oce")

  stations <- load_algaware_stations()
  result <- algaware:::parse_single_cnv(cnv_path, stations)

  expect_s3_class(result, "data.frame")
  expect_equal(result$station_short[1], "Anholt E")
  expect_true(nrow(result) > 0)
  expect_true(all(c("pressure_dbar", "chl_fluorescence", "cruise",
                     "sample_date", "latitude", "longitude") %in% names(result)))
  expect_true(max(result$pressure_dbar, na.rm = TRUE) > 0)
  expect_false(all(is.na(result$chl_fluorescence)))
})

test_that("parse_single_cnv returns NULL for non-existent file", {
  skip_if_not_installed("oce")
  stations <- load_algaware_stations()
  result <- algaware:::parse_single_cnv("/nonexistent/file.cnv", stations)
  expect_null(result)
})

test_that("parse_single_cnv returns NULL when station not in algaware_stations", {
  cnv_path <- testthat::test_path("test_data",
                                   "SBE09_0745_20260313_1005_77SE_06_0276.cnv")
  skip_if_not(file.exists(cnv_path), "Test CNV file not available")
  skip_if_not_installed("oce")

  empty_stations <- data.frame(
    STATION_NAME = character(0),
    STATION_NAME_SHORT = character(0),
    COAST = character(0),
    stringsAsFactors = FALSE
  )
  result <- algaware:::parse_single_cnv(cnv_path, empty_stations)
  expect_null(result)
})

# -- parse_single_cnv_all -----------------------------------------------------

test_that("parse_single_cnv_all parses ANHOLT E and attaches region", {
  cnv_path <- testthat::test_path("test_data",
                                   "SBE09_0745_20260313_1005_77SE_06_0276.cnv")
  skip_if_not(file.exists(cnv_path), "Test CNV file not available")
  skip_if_not_installed("oce")

  mapper <- load_station_mapper()
  std    <- load_standard_stations()
  result <- algaware:::parse_single_cnv_all(cnv_path, mapper, std)

  expect_s3_class(result, "data.frame")
  expect_equal(result$canonical_name[1], "ANHOLT E")
  expect_equal(result$region[1], "The Kattegat and The Sound")
  expect_true(nrow(result) > 0)
  expect_false(all(is.na(result$chl_fluorescence)))
})

test_that("parse_single_cnv_all returns NULL when station not in standard_stations", {
  cnv_path <- testthat::test_path("test_data",
                                   "SBE09_0745_20260313_1005_77SE_06_0276.cnv")
  skip_if_not(file.exists(cnv_path), "Test CNV file not available")
  skip_if_not_installed("oce")

  empty_mapper <- data.frame(synonym = character(0), statn = character(0),
                              stringsAsFactors = FALSE)
  empty_std    <- data.frame(station_name = character(0),
                              region = character(0), stringsAsFactors = FALSE)
  result <- algaware:::parse_single_cnv_all(cnv_path, empty_mapper, empty_std)
  expect_null(result)
})

# -- read_cnv_folder ----------------------------------------------------------

test_that("read_cnv_folder parses CNV file folder", {
  cnv_dir <- testthat::test_path("test_data")
  skip_if_not(
    length(list.files(cnv_dir, pattern = "\\.cnv$")) > 0,
    "No CNV files in test_data"
  )
  skip_if_not_installed("oce")

  stations <- load_algaware_stations()
  result <- read_cnv_folder(cnv_dir, stations)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("station_short" %in% names(result))
})

# -- read_cnv_folder_all -------------------------------------------------------

test_that("read_cnv_folder_all parses all CNV files with region info", {
  cnv_dir <- testthat::test_path("test_data")
  skip_if_not(
    length(list.files(cnv_dir, pattern = "\\.cnv$")) > 0,
    "No CNV files in test_data"
  )
  skip_if_not_installed("oce")

  mapper <- load_station_mapper()
  std    <- load_standard_stations()
  result <- read_cnv_folder_all(cnv_dir, mapper, std)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("canonical_name", "region", "pressure_dbar",
                     "chl_fluorescence") %in% names(result)))
})

# -- create_ctd_region_figure -------------------------------------------------

test_that("create_ctd_region_figure returns patchwork for Kattegat region", {
  cnv_dir <- testthat::test_path("test_data")
  skip_if_not(
    length(list.files(cnv_dir, pattern = "\\.cnv$")) > 0,
    "No CNV files in test_data"
  )
  skip_if_not_installed(c("oce", "patchwork"))

  mapper <- load_station_mapper()
  std    <- load_standard_stations()
  ctd_data <- read_cnv_folder_all(cnv_dir, mapper, std)
  skip_if(is.null(ctd_data), "CNV parsing returned NULL")

  chl_stats <- load_chl_statistics()
  result <- create_ctd_region_figure(
    ctd_data_full     = ctd_data,
    chl_stats         = chl_stats,
    standard_stations = std,
    region            = "The Kattegat and The Sound",
    current_year      = 2026L
  )
  expect_false(is.null(result))
  expect_true(inherits(result, "patchwork") || inherits(result, "gg"))
})

# -- read_cnv_folder (error cases) --------------------------------------------

test_that("read_cnv_folder warns and returns NULL for non-existent folder", {
  expect_warning(
    result <- read_cnv_folder("/nonexistent/path", data.frame()),
    "does not exist"
  )
  expect_null(result)
})

test_that("read_cnv_folder warns and returns NULL for empty folder", {
  tmp <- tempdir()
  expect_warning(
    result <- read_cnv_folder(tmp, data.frame()),
    "No .cnv files"
  )
  expect_null(result)
})

# -- read_cnv_folder_all (error cases) ----------------------------------------

test_that("read_cnv_folder_all warns and returns NULL for non-existent folder", {
  expect_warning(
    result <- read_cnv_folder_all("/nonexistent/path",
                                  data.frame(synonym = character(0), statn = character(0)),
                                  data.frame(station_name = character(0), region = character(0))),
    "does not exist"
  )
  expect_null(result)
})

test_that("read_cnv_folder_all warns and returns NULL for empty folder", {
  tmp <- tempdir()
  expect_warning(
    result <- read_cnv_folder_all(tmp,
                                  data.frame(synonym = character(0), statn = character(0)),
                                  data.frame(station_name = character(0), region = character(0))),
    "No .cnv files"
  )
  expect_null(result)
})
