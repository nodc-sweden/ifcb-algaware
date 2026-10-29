# -- Fixtures -------------------------------------------------------------------

# Baltic-only IFCB station summary (two visits).
baltic_summary <- function() {
  mk <- function(name, short, date, lat, lon) data.frame(
    STATION_NAME = name, STATION_NAME_SHORT = short, COAST = "EAST",
    LATITUDE_WGS84_SWEREF99_DD = lat, LONGITUDE_WGS84_SWEREF99_DD = lon,
    visit_date = as.Date(date), visit_id = paste0(short, "_", date),
    median_time = as.POSIXct(paste(date, "12:00"), tz = "UTC"),
    name = c("Skeletonema marinoi", "Aphanizomenon flosaquae"),
    sflag = "", AphiaID = c(149075L, 146563L), n_samples = 2L,
    biovolume_mm3_per_liter = c(0.5, 0.3), carbon_ug_per_liter = c(10, 5),
    counts_per_liter = c(1000, 500), chl_mean = 2,
    stringsAsFactors = FALSE
  )
  rbind(mk("BY5 BORNHOLMSDJ", "BY5", "2026-10-24", 55.25, 15.98),
        mk("BY15 GOTLANDSDJ", "BY15", "2026-10-25", 57.32, 20.05))
}

baltic_groups <- function() {
  data.frame(name = c("Skeletonema marinoi", "Aphanizomenon flosaquae"),
             AphiaID = c(149075L, 146563L),
             phyto_group = c("Diatoms", "Cyanobacteria"),
             stringsAsFactors = FALSE)
}

# The Släggö fixture, station-matched and enriched without network access.
slaggo_microscopy <- function() {
  path <- testthat::test_path("test_data",
                              "Slaggo_2026-10-26_0-10m.xlsx")
  loaded <- load_microscopy_samples(path, basename(path),
                                    load_algaware_stations(), data.frame())
  local_mocked_bindings(
    fetch_worms_records = function(names) stop("offline"),
    fetch_hab_list = function() {
      data.frame(AphiaID = NA_integer_,
                 name = "Dolichospermum planctonicum",
                 stringsAsFactors = FALSE)
    },
    assign_phyto_groups = function(...) stop("offline")
  )
  enrich_microscopy_taxa(loaded$obs)$obs
}

docx_text <- function(path) officer::docx_summary(officer::read_docx(path))$text

# -- microscopy_intro_text ------------------------------------------------------

test_that("microscopy_intro_text names the stations and the method", {
  obs <- slaggo_microscopy()
  text <- microscopy_intro_text(obs)
  expect_match(text, "No IFCB data were collected at Släggö", fixed = TRUE)
  expect_match(text, "light microscopy analysis of a 0–10 m hose sample",
               fixed = TRUE)
  expect_match(text, "analysed by Anders Torstensson", fixed = TRUE)
  expect_match(text, "not included in the figures", fixed = TRUE)
  expect_equal(microscopy_intro_text(NULL), "")
})

test_that("microscopy_intro_text uses the plural for several stations", {
  obs <- slaggo_microscopy()
  other <- obs
  other$STATION_NAME_SHORT <- "Å17"
  other$visit_id <- "Å17_2026-10-26_microscopy"
  text <- microscopy_intro_text(rbind(obs, other))
  expect_match(text, "Släggö and Å17; these stations are described",
               fixed = TRUE)
  expect_match(text, "analyses of 0–10 m hose samples", fixed = TRUE)
})

# -- build_summary_rows ---------------------------------------------------------

test_that("build_summary_rows lists microscopy samples", {
  rows <- build_summary_rows(microscopy = slaggo_microscopy())
  expect_equal(rows$Parameter, "Light microscopy samples (qualitative)")
  expect_equal(rows$Value, "Släggö (2026-10-26)")
  expect_equal(nrow(build_summary_rows()), 0L)
})

# -- build_report_taxa_lookup without an IFCB lookup ----------------------------

test_that("build_report_taxa_lookup works without an IFCB lookup", {
  out <- build_report_taxa_lookup(NULL, slaggo_microscopy())
  expect_equal(nrow(out), 23L)
  expect_true(out$HAB[out$name == "Dolichospermum planctonicum"])
})

# -- add_station_sections -------------------------------------------------------

test_that("add_station_sections merges microscopy stations in region order", {
  doc <- add_station_sections(officer::read_docx(), baltic_summary(),
                              microscopy = slaggo_microscopy())
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  print(doc, target = path)
  text <- docx_text(path)

  headings <- text[text %in% c("West Coast", "Baltic Sea") |
                     grepl(" - 2026-", text)]
  expect_equal(headings, c("West Coast",
                           "Släggö - 2026-10-26 (microscopy)",
                           "Baltic Sea", "BY5 - 2026-10-24",
                           "BY15 - 2026-10-25"))
})

test_that("generate_station_descriptions uses the microscopy prompt", {
  prompts <- NULL
  local_mocked_bindings(
    call_llm_batch = function(p, ...) {
      prompts <<- p
      lapply(p, function(x) list(text = "ok", error = NULL))
    },
    llm_supports_parallel = function(...) TRUE
  )
  mic <- slaggo_microscopy()
  visits <- rbind(
    data.frame(unique(baltic_summary()[1, c("STATION_NAME",
                                            "STATION_NAME_SHORT", "COAST",
                                            "visit_date", "visit_id")]),
               source = "ifcb"),
    data.frame(unique(mic[, c("STATION_NAME", "STATION_NAME_SHORT", "COAST",
                              "visit_date", "visit_id")]),
               source = "microscopy")
  )
  out <- generate_station_descriptions(visits, baltic_summary(), NULL,
                                       use_llm = TRUE, microscopy = mic)
  expect_equal(out, c("ok", "ok"))
  expect_false(grepl("light microscopy", prompts[[1]]$user))
  expect_match(prompts[[2]]$user, "light microscopy", fixed = TRUE)
  expect_match(prompts[[2]]$user, "Dolichospermum planctonicum", fixed = TRUE)
})

# -- generate_report end to end -------------------------------------------------

test_that("generate_report fills the missing coast with microscopy text", {
  ss <- baltic_summary()
  mic <- slaggo_microscopy()
  figure_input <- NULL
  local_mocked_bindings(create_biomass_maps = function(station_summary) {
    figure_input <<- station_summary
    list()
  })

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressWarnings(generate_report(
    out, ss, create_wide_summary(ss, "EAST"), create_wide_summary(ss, "WEST"),
    use_llm = FALSE, cruise_info = "test", phyto_groups = baltic_groups(),
    microscopy = mic
  ))
  text <- docx_text(out)

  expect_true("Släggö - 2026-10-26 (microscopy)" %in% text)
  expect_true("Baltic Sea - Biovolume heatmap" %in% text)
  expect_false("West Coast - Biovolume heatmap" %in% text)
  expect_true(any(grepl("No IFCB data were collected at Släggö", text)))
  expect_true("Light microscopy samples (qualitative)" %in% text)
  # Figures never see microscopy data
  expect_false(any(mic$name %in% figure_input$name))
  expect_false("WEST" %in% figure_input$COAST)
})

test_that("generate_report passes microscopy to the summary prompt", {
  ss <- baltic_summary()
  prompts <- character(0)
  local_mocked_bindings(
    call_llm = function(system_prompt, user_prompt, ...) {
      prompts <<- c(prompts, user_prompt)
      "Summary text."
    },
    call_llm_batch = function(p, ...) {
      lapply(p, function(x) list(text = "Station text.", error = NULL))
    },
    llm_supports_parallel = function(...) TRUE
  )
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressWarnings(generate_report(
    out, ss, create_wide_summary(ss, "EAST"), create_wide_summary(ss, "WEST"),
    use_llm = TRUE, llm_provider = "claude", cruise_info = "test",
    phyto_groups = baltic_groups(), microscopy = slaggo_microscopy()
  ))
  summary_prompt <- prompts[grepl("English summary", prompts)]
  expect_length(summary_prompt, 1L)
  expect_match(summary_prompt, "Microscopy stations", fixed = TRUE)
  expect_match(summary_prompt, "The West Coast stations have no IFCB data",
               fixed = TRUE)
})
