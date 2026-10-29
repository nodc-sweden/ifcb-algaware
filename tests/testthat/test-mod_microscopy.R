slaggo_file <- function() {
  testthat::test_path("test_data", "Slaggo_2026-10-26_0-10m.xlsx")
}

baltic_ifcb <- function() {
  data.frame(STATION_NAME = "BY5 BORNHOLMSDJ", STATION_NAME_SHORT = "BY5",
             COAST = "EAST", visit_date = as.Date("2026-10-24"),
             visit_id = "BY5_2026-10-24", name = "Skeletonema marinoi",
             biovolume_mm3_per_liter = 0.5, stringsAsFactors = FALSE)
}

offline_taxon_services <- function(env = parent.frame()) {
  local_mocked_bindings(
    fetch_worms_records = function(names) stop("offline"),
    fetch_hab_list = function() stop("offline"),
    assign_phyto_groups = function(...) stop("offline"),
    .env = env
  )
}

# -- microscopy_status_items ----------------------------------------------------

test_that("microscopy_status_items describes used and skipped samples", {
  status <- data.frame(
    sample_index = 1:3, file = c("a.xlsx", "b.xlsx", "c.xlsx"),
    station = c("Släggö", "BY5", NA),
    date = as.Date(c("2026-10-26", "2026-10-25", NA)),
    used = c(TRUE, FALSE, FALSE),
    reason = c("", "IFCB data exists for this station",
               "could not be read: bad file"),
    stringsAsFactors = FALSE
  )
  items <- microscopy_status_items(status)
  expect_length(items, 3L)
  expect_true(items[[1]]$ok)
  expect_equal(items[[1]]$label, "Släggö 2026-10-26: used")
  expect_false(items[[2]]$ok)
  expect_equal(items[[2]]$label,
               "BY5 2026-10-25: skipped, IFCB data exists for this station")
  expect_equal(items[[3]]$label, "c.xlsx: skipped, could not be read: bad file")
  expect_length(microscopy_status_items(NULL), 0L)
})

# -- report_cruise_info ---------------------------------------------------------

test_that("report_cruise_info widens the dates to the microscopy samples", {
  times <- as.POSIXct(c("2026-10-20 10:00", "2026-10-24 10:00"), tz = "UTC")
  mic <- data.frame(visit_date = as.Date("2026-10-26"))
  expect_equal(report_cruise_info("RV Svea October cruise, 2026-10-20 to 2026-10-24",
                                  times, mic),
               "RV Svea October cruise, 2026-10-20 to 2026-10-26")
  expect_equal(report_cruise_info("unchanged", times, NULL), "unchanged")
})

# -- build_readiness_items ------------------------------------------------------

test_that("build_readiness_items lists microscopy stations when present", {
  items <- build_readiness_items(TRUE, FALSE, NULL, NULL,
                                 microscopy_stations = "Släggö")
  expect_length(items, 5L)
  expect_true(items[[5]]$ok)
  expect_match(items[[5]]$label, "Släggö", fixed = TRUE)
  expect_length(build_readiness_items(TRUE, FALSE, NULL, NULL,
                                      microscopy_stations = character(0)), 4L)
})

# -- prepare_report_microscopy --------------------------------------------------

test_that("prepare_report_microscopy selects gap samples and adds taxon data", {
  offline_taxon_services()
  loaded <- load_microscopy_samples(slaggo_file(), "s.xlsx",
                                    load_algaware_stations(), data.frame())
  res <- prepare_report_microscopy(loaded, baltic_ifcb(), NULL)
  expect_equal(unique(res$obs$STATION_NAME_SHORT), "Släggö")
  expect_true(all(c("HAB", "italic", "phyto_group") %in% names(res$obs)))
  expect_true(any(grepl("HAB list", res$notes)))
})

test_that("prepare_report_microscopy returns NULL when nothing is used", {
  offline_taxon_services()
  expect_null(prepare_report_microscopy(NULL, baltic_ifcb(), NULL)$obs)

  loaded <- load_microscopy_samples(slaggo_file(), "s.xlsx",
                                    load_algaware_stations(), data.frame())
  ifcb_at_slaggo <- baltic_ifcb()
  ifcb_at_slaggo$STATION_NAME <- "SLÄGGÖ"
  expect_null(prepare_report_microscopy(loaded, ifcb_at_slaggo, NULL)$obs)
})

# -- mod_microscopy_server ------------------------------------------------------

test_that("mod_microscopy_server loads uploads and shows their status", {
  rv <- shiny::reactiveValues(station_summary = baltic_ifcb(),
                              microscopy_loaded = NULL)
  config <- list(extra_stations = list())

  shiny::testServer(mod_microscopy_server,
                    args = list(rv = rv, config = config), {
    session$setInputs(files = data.frame(
      name = basename(slaggo_file()), size = 1L, type = "",
      datapath = slaggo_file(), stringsAsFactors = FALSE
    ))
    expect_true(rv$microscopy_loaded$status$used)
    expect_equal(unique(rv$microscopy_loaded$obs$COAST), "WEST")
    expect_match(output$status$html, "Släggö 2026-10-26: used",
                 fixed = TRUE)

    # IFCB data for the same station makes the sample a skipped one
    ifcb <- baltic_ifcb()
    ifcb$STATION_NAME <- "SLÄGGÖ"
    rv$station_summary <- ifcb
    session$flushReact()
    expect_match(output$status$html, "IFCB data exists", fixed = TRUE)

    session$setInputs(clear = 1)
    expect_null(rv$microscopy_loaded)
  })
})
