test_that("load_algaware_stations returns a data.frame", {
  stations <- load_algaware_stations()
  expect_s3_class(stations, "data.frame")
  expect_true("STATION_NAME" %in% names(stations))
  expect_true("COAST" %in% names(stations))
  expect_true("STATION_NAME_SHORT" %in% names(stations))
  expect_true(nrow(stations) > 0)
})

test_that("load_algaware_stations reads Swedish station names as UTF-8", {
  # Regression: on non-UTF-8 locales (e.g. Windows Server) station names with
  # Å/Ä/Ö were mangled and silently dropped from the spatial join. Names must
  # be valid UTF-8 with the expected characters regardless of host locale.
  stations <- load_algaware_stations()

  expect_true("Å17" %in% stations$STATION_NAME)
  expect_true("SLÄGGÖ" %in% stations$STATION_NAME)

  swedish <- stations$STATION_NAME[grepl("Å17|SLÄGGÖ", stations$STATION_NAME)]
  expect_true(all(Encoding(swedish) == "UTF-8"))
  expect_false(any(is.na(iconv(stations$STATION_NAME, "UTF-8", "UTF-8"))))
})

test_that("as_utf8_columns marks character columns as UTF-8", {
  df <- data.frame(
    a = "SL\xc4GG\xd6",          # latin1 bytes for SLÄGGÖ
    b = 1L,
    stringsAsFactors = FALSE
  )
  Encoding(df$a) <- "latin1"

  out <- algaware:::as_utf8_columns(df)
  expect_equal(Encoding(out$a), "UTF-8")
  expect_equal(out$a, "SLÄGGÖ")
  expect_type(out$b, "integer")
})

test_that("latin1-bundled files are read with Å/Ä/Ö intact (UTF-8)", {
  mapper <- load_station_mapper()
  expect_s3_class(mapper, "data.frame")
  if (nrow(mapper) > 0) {
    char_cols <- vapply(mapper, is.character, logical(1))
    encs <- unlist(lapply(mapper[char_cols], Encoding))
    # No column should contain mojibake / unknown-marked non-ASCII bytes.
    expect_false(any(encs == "latin1"))
    expect_true(any(grepl("SLÄGGÖ", mapper[[1]], fixed = TRUE)))
  }

  chl <- load_chl_statistics()
  expect_s3_class(chl, "data.frame")
  if (nrow(chl) > 0) {
    expect_false(any(is.na(iconv(chl$STATN, "UTF-8", "UTF-8"))))
  }
})

test_that("load_algaware_stations appends extra stations", {
  base <- load_algaware_stations()
  extra <- list(
    list(STATION_NAME = "TEST_STATION_XYZ",
         COAST = "EAST",
         STATION_NAME_SHORT = "TST")
  )

  result <- load_algaware_stations(extra_stations = extra)
  expect_equal(nrow(result), nrow(base) + 1)
  expect_true("TEST_STATION_XYZ" %in% result$STATION_NAME)
})

test_that("load_algaware_stations does not duplicate existing stations", {
  base <- load_algaware_stations()
  existing_name <- base$STATION_NAME[1]

  extra <- list(
    list(STATION_NAME = existing_name,
         COAST = "EAST",
         STATION_NAME_SHORT = "DUP")
  )

  result <- load_algaware_stations(extra_stations = extra)
  expect_equal(nrow(result), nrow(base))
})

test_that("assign_station_visits assigns visit IDs", {
  metadata <- data.frame(
    STATION_NAME = c("STN_A", "STN_A", "STN_A", "STN_B"),
    sample_time = as.POSIXct(c(
      "2022-01-01 10:00:00",
      "2022-01-01 11:00:00",
      "2022-01-03 10:00:00",
      "2022-01-01 10:00:00"
    )),
    stringsAsFactors = FALSE
  )

  result <- algaware:::assign_station_visits(metadata, max_gap_hours = 12)
  expect_true("visit_id" %in% names(result))
  expect_false(any(is.na(result$visit_id)))

  # First two bins at STN_A should be same visit, third a different one
  stn_a <- result[result$STATION_NAME == "STN_A", ]
  expect_equal(stn_a$visit_id[1], stn_a$visit_id[2])
  expect_false(stn_a$visit_id[2] == stn_a$visit_id[3])
})

test_that("assign_station_visits handles single bin per station", {
  metadata <- data.frame(
    STATION_NAME = "STN_A",
    sample_time = as.POSIXct("2022-01-01 10:00:00"),
    stringsAsFactors = FALSE
  )

  result <- algaware:::assign_station_visits(metadata)
  expect_equal(result$visit_id, "STN_A_visit1")
})
