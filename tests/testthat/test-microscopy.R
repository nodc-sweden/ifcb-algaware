# -- Fixtures -------------------------------------------------------------------

slaggo_path <- function() {
  testthat::test_path("test_data", "Slaggo_2026-10-26_0-10m.xlsx")
}

# Minimal Plankton Toolbox sheets, shaped like SHARK4R::read_ptbx() output.
ptbx_info <- function(station = "Släggö", date = "2026-10-26",
                      lat = "58.25985", lon = "11.43572") {
  data.frame(
    key = c("sample_date", "station_name", "sample_latitude_dd",
            "sample_longitude_dd", "sample_min_depth_m", "sample_max_depth_m",
            "sampler_type_code", "analysed_by"),
    value = c(date, station, lat, lon, "0", "10", "HOS (Hose)", "A. Analyst"),
    stringsAsFactors = FALSE
  )
}

ptbx_data <- function(names = c("Dinophysis acuminata", "Skeletonema marinoi"),
                      classes = c("2", "5")) {
  data.frame(
    scientific_full_name = names,
    scientific_name = names,
    taxon_class = c("Dinophyceae", "Bacillariophyceae")[seq_along(names)],
    trophic_type = "AU",
    abundance_class = classes,
    stringsAsFactors = FALSE
  )
}

ptbx_method <- function(qual = "Qualitative") {
  data.frame(counting_method = "B - Live_net_sample",
             qualitative_quantitative = qual, stringsAsFactors = FALSE)
}

# A small SHARK register so station matching never depends on the bundle.
shark_fixture <- function() {
  data.frame(
    STATION_NAME = c("SLÄGGÖ", "BY5 BORNHOLMSDJ"),
    SYNONYM_NAMES = c("TRÖSKELN<or>SK25", "BY 5<or>BORNHOLMSDJUPET"),
    LATITUDE_WGS84_SWEREF99_DD = c(58.25984, 55.25),
    LONGITUDE_WGS84_SWEREF99_DD = c(11.43567, 15.98333),
    OUT_OF_BOUNDS_RADIUS = c(1200, 2000),
    stringsAsFactors = FALSE
  )
}

stations_fixture <- function() {
  data.frame(
    STATION_NAME = c("SLÄGGÖ", "BY5 BORNHOLMSDJ"),
    COAST = c("WEST", "EAST"),
    STATION_NAME_SHORT = c("Släggö", "BY5"),
    stringsAsFactors = FALSE
  )
}

ifcb_summary <- function(stations = "BY5 BORNHOLMSDJ",
                         dates = as.Date("2026-10-24")) {
  data.frame(
    STATION_NAME = stations,
    STATION_NAME_SHORT = sub(" .*", "", stations),
    COAST = "EAST",
    visit_date = dates,
    visit_id = paste0(sub(" .*", "", stations), "_", dates),
    name = "Skeletonema marinoi",
    biovolume_mm3_per_liter = 0.5,
    stringsAsFactors = FALSE
  )
}

# -- read_microscopy_sample / parse_ptbx_sample ---------------------------------

test_that("read_microscopy_sample reads the Plankton Toolbox export", {
  obs <- read_microscopy_sample(slaggo_path())

  expect_equal(nrow(obs), 23L)
  expect_equal(unique(obs$station_reported), "Släggö")
  expect_equal(unique(obs$sample_date), as.Date("2026-10-26"))
  expect_equal(unique(obs$min_depth_m), 0)
  expect_equal(unique(obs$max_depth_m), 10)
  expect_equal(unique(obs$sampler), "HOS (Hose)")
  expect_true(all(obs$abundance_class %in% 1:5))

  doli <- obs[obs$name == "Dolichospermum planctonicum", ]
  expect_equal(doli$abundance_class, 5L)
  expect_equal(doli$abundance_label, "50-100%")
  expect_equal(doli$taxon_class, "Cyanophyceae")
})

test_that("parse_ptbx_sample labels the 1-5 scale and sorts by class", {
  obs <- parse_ptbx_sample(ptbx_info(), ptbx_data(), ptbx_method(), "s.xlsx")
  expect_equal(obs$name, c("Skeletonema marinoi", "Dinophysis acuminata"))
  expect_equal(obs$abundance_label, c("50-100%", "Several cells"))
  expect_equal(unique(obs$sample_file), "s.xlsx")
  expect_equal(unique(obs$latitude), 58.25985)
})

test_that("parse_ptbx_sample keeps the highest class of a repeated taxon", {
  data <- ptbx_data(c("Skeletonema marinoi", "Skeletonema marinoi"),
                    c("2", "4"))
  obs <- parse_ptbx_sample(ptbx_info(), data, ptbx_method(), "s.xlsx")
  expect_equal(nrow(obs), 1L)
  expect_equal(obs$abundance_class, 4L)
})

test_that("parse_ptbx_sample rejects quantitative samples", {
  expect_error(
    parse_ptbx_sample(ptbx_info(), ptbx_data(), ptbx_method("Quantitative"),
                      "s.xlsx"),
    "qualitative"
  )
})

test_that("parse_ptbx_sample rejects missing station, date or classes", {
  expect_error(parse_ptbx_sample(ptbx_info(station = ""), ptbx_data(),
                                 ptbx_method(), "s.xlsx"), "station")
  expect_error(parse_ptbx_sample(ptbx_info(date = "not a date"), ptbx_data(),
                                 ptbx_method(), "s.xlsx"), "date")
  expect_error(parse_ptbx_sample(ptbx_info(), ptbx_data(classes = c("2", "7")),
                                 ptbx_method(), "s.xlsx"), "abundance class")
  expect_error(parse_ptbx_sample(ptbx_info(), ptbx_data()[0, ],
                                 ptbx_method(), "s.xlsx"), "no taxa")
})

# -- match_microscopy_station ---------------------------------------------------

test_that("match_microscopy_station matches names regardless of case", {
  m <- match_microscopy_station("Släggö", NA, NA,
                                stations_fixture(), shark_fixture())
  expect_equal(m$STATION_NAME, "SLÄGGÖ")
  expect_equal(m$COAST, "WEST")
  expect_equal(m$match_method, "name")

  m <- match_microscopy_station("by5", NA, NA,
                                stations_fixture(), shark_fixture())
  expect_equal(m$STATION_NAME, "BY5 BORNHOLMSDJ")
})

test_that("match_microscopy_station falls back to SHARK synonyms", {
  m <- match_microscopy_station("Tröskeln", NA, NA,
                                stations_fixture(), shark_fixture())
  expect_equal(m$STATION_NAME, "SLÄGGÖ")
  expect_equal(m$match_method, "synonym")
})

test_that("match_microscopy_station falls back to the sample position", {
  m <- match_microscopy_station("Unknown name", 55.251, 15.984,
                                stations_fixture(), shark_fixture())
  expect_equal(m$STATION_NAME, "BY5 BORNHOLMSDJ")
  expect_equal(m$match_method, "position")
})

test_that("match_microscopy_station returns NA when nothing matches", {
  m <- match_microscopy_station("Unknown name", 57, 18,
                                stations_fixture(), shark_fixture())
  expect_true(is.na(m$STATION_NAME))
  m <- match_microscopy_station("Unknown name", NA, NA,
                                stations_fixture(), shark_fixture())
  expect_true(is.na(m$STATION_NAME))
})

# -- load_microscopy_samples ----------------------------------------------------

test_that("load_microscopy_samples matches stations and reports failures", {
  bad <- tempfile(fileext = ".xlsx")
  writeLines("not an excel file", bad)
  on.exit(unlink(bad), add = TRUE)

  loaded <- load_microscopy_samples(
    c(slaggo_path(), bad), c("slaggo.xlsx", "broken.xlsx"),
    stations_fixture(), shark_fixture()
  )

  expect_equal(nrow(loaded$status), 2L)
  expect_equal(loaded$status$used, c(TRUE, FALSE))
  expect_match(loaded$status$reason[2], "could not be read")

  expect_equal(unique(loaded$obs$STATION_NAME), "SLÄGGÖ")
  expect_equal(unique(loaded$obs$COAST), "WEST")
  expect_equal(unique(loaded$obs$visit_id),
               "Släggö_2026-10-26_microscopy")
})

test_that("load_microscopy_samples skips samples from unknown stations", {
  local_mocked_bindings(read_microscopy_sample = function(path, file_name) {
    parse_ptbx_sample(ptbx_info(station = "Nowhere", lat = "57", lon = "18"),
                      ptbx_data(), ptbx_method(), file_name)
  })
  loaded <- load_microscopy_samples("x.xlsx", "x.xlsx",
                                    stations_fixture(), shark_fixture())
  expect_false(loaded$status$used)
  expect_match(loaded$status$reason, "not an AlgAware station")
  expect_equal(nrow(loaded$obs), 0L)
})

# -- select_gap_samples ---------------------------------------------------------

load_two <- function(stations = c("Släggö", "BY5"),
                     dates = c("2026-10-26", "2026-10-25")) {
  i <- 0L
  local_mocked_bindings(
    read_microscopy_sample = function(path, file_name) {
      i <<- i + 1L
      parse_ptbx_sample(ptbx_info(station = stations[i], date = dates[i]),
                        ptbx_data(), ptbx_method(), file_name)
    }
  )
  load_microscopy_samples(c("a.xlsx", "b.xlsx"), c("a.xlsx", "b.xlsx"),
                          stations_fixture(), shark_fixture())
}

test_that("select_gap_samples gives IFCB data precedence", {
  sel <- select_gap_samples(load_two(), ifcb_summary())
  expect_equal(sel$status$used, c(TRUE, FALSE))
  expect_match(sel$status$reason[2], "IFCB data")
  expect_equal(unique(sel$obs$STATION_NAME), "SLÄGGÖ")
})

test_that("select_gap_samples skips samples far outside the cruise dates", {
  loaded <- load_two(dates = c("2026-10-26", "2026-09-01"))
  sel <- select_gap_samples(loaded, ifcb_summary(stations = "OTHER"))
  expect_equal(sel$status$used, c(TRUE, FALSE))
  expect_match(sel$status$reason[2], "outside the cruise dates")
})

test_that("select_gap_samples keeps one sample per station", {
  loaded <- load_two(stations = c("Släggö", "Släggö"),
                     dates = c("2026-10-26", "2026-10-26"))
  sel <- select_gap_samples(loaded, ifcb_summary())
  expect_equal(sel$status$used, c(TRUE, FALSE))
  expect_match(sel$status$reason[2], "already used")
  expect_equal(unique(sel$obs$sample_file), "a.xlsx")
})

test_that("select_gap_samples skips the IFCB checks without IFCB data", {
  sel <- select_gap_samples(load_two(), NULL)
  expect_equal(sel$status$used, c(TRUE, TRUE))
})

# -- enrich_microscopy_taxa -----------------------------------------------------

enrich_obs <- function() {
  data.frame(
    name = c("Dinophysis acuminata", "Skeletonema marinoi", "Centrales",
             "Pseudo-nitzschia", "Dictyocha speculum"),
    display_name = c("Dinophysis acuminata", "Skeletonema marinoi",
                     "Centrales", "Pseudo-nitzschia", "Dictyocha speculum"),
    taxon_class = c("Dinophyceae", "Bacillariophyceae", "Bacillariophyceae",
                    "Bacillariophyceae", "Dictyochophyceae"),
    abundance_class = c(2L, 5L, 2L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

mock_taxon_services <- function(worms = TRUE, hab = TRUE, groups = TRUE,
                                env = parent.frame()) {
  local_mocked_bindings(
    fetch_worms_records = function(names) {
      if (!worms) stop("WoRMS offline")
      data.frame(name = names, AphiaID = seq_along(names),
                 rank = ifelse(names == "Centrales", "Order",
                               ifelse(grepl(" ", names), "Species", "Genus")),
                 stringsAsFactors = FALSE)
    },
    fetch_hab_list = function() {
      if (!hab) stop("HAB list offline")
      data.frame(AphiaID = 1L,
                 name = c("Dinophysis acuminata", "Pseudo-nitzschia seriata"),
                 stringsAsFactors = FALSE)
    },
    assign_phyto_groups = function(scientific_names, aphia_ids = NULL,
                                   verbose = FALSE) {
      if (!groups) stop("groups offline")
      ifelse(grepl("Dinophysis", scientific_names), "Dinoflagellates",
             ifelse(grepl("Dictyocha", scientific_names),
                    "Silicoflagellates", "Diatoms"))
    },
    .env = env
  )
}

test_that("enrich_microscopy_taxa flags HAB taxa, italics and groups", {
  mock_taxon_services()
  res <- enrich_microscopy_taxa(enrich_obs())
  obs <- res$obs

  # Exact HAB-list matches only: a genus is not flagged because one of its
  # species is on the list (the IFCB lookup flags genera case by case).
  expect_equal(obs$HAB, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(obs$italic, c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_equal(obs$phyto_group, c("Dinoflagellates", "Diatoms", "Diatoms",
                                  "Diatoms", "Silicoflagellates"))
  expect_equal(res$notes, character(0))
})

test_that("enrich_microscopy_taxa prefers HAB flags from the taxa lookup", {
  mock_taxon_services()
  lookup <- data.frame(
    name = c("Skeletonema marinoi", "Pseudo-nitzschia", "Pseudo-nitzschia",
             "Dinophysis acuminata"),
    HAB = c(TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  res <- enrich_microscopy_taxa(enrich_obs(), lookup)
  hab <- stats::setNames(res$obs$HAB, res$obs$name)
  expect_true(hab[["Skeletonema marinoi"]])
  # Any HAB row for the name counts (sp. and spp. rows can differ)
  expect_true(hab[["Pseudo-nitzschia"]])
  # The curated lookup wins over the HAB list
  expect_false(hab[["Dinophysis acuminata"]])
})

test_that("enrich_microscopy_taxa falls back when services are offline", {
  mock_taxon_services(worms = FALSE, hab = FALSE, groups = FALSE)
  res <- enrich_microscopy_taxa(enrich_obs())
  obs <- res$obs

  # Groups from the taxon class via phyto_groups.yaml
  expect_equal(obs$phyto_group, c("Dinoflagellates", "Diatoms", "Diatoms",
                                  "Diatoms", "Silicoflagellates"))
  # Italics from the name: "Centrales" ends like an order
  expect_equal(obs$italic, c(TRUE, TRUE, FALSE, TRUE, TRUE))
  expect_true(all(is.na(obs$AphiaID)))
  expect_false(any(obs$HAB))
  expect_true(any(grepl("HAB list", res$notes)))
})

# -- build_report_taxa_lookup ---------------------------------------------------

test_that("build_report_taxa_lookup appends microscopy taxa", {
  lookup <- data.frame(
    clean_names = "Skeletonema_marinoi", name = "Skeletonema marinoi",
    sflag = "", AphiaID = 149075L, HAB = FALSE, warning_level = NA_real_,
    italic = TRUE, is_diatom = TRUE, stringsAsFactors = FALSE
  )
  obs <- enrich_obs()
  obs$AphiaID <- seq_len(nrow(obs))
  obs$HAB <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  obs$italic <- c(TRUE, TRUE, FALSE, TRUE, TRUE)
  obs$phyto_group <- c("Dinoflagellates", "Diatoms", "Diatoms", "Diatoms",
                       "Silicoflagellates")

  out <- build_report_taxa_lookup(lookup, obs)

  expect_equal(nrow(out), 5L)
  expect_equal(names(out), names(lookup))
  # The curated IFCB row wins for taxa already in the lookup
  expect_false(out$HAB[out$name == "Skeletonema marinoi"])
  added <- out[out$name == "Dinophysis acuminata", ]
  expect_true(added$HAB)
  expect_true(added$italic)
  expect_false(added$is_diatom)
  expect_true(is.na(added$warning_level))
  expect_false(any(out$clean_names[-1] %in% lookup$clean_names))
})

test_that("build_report_taxa_lookup returns the lookup without microscopy", {
  lookup <- data.frame(name = "A b", HAB = FALSE, stringsAsFactors = FALSE)
  expect_identical(build_report_taxa_lookup(lookup, NULL), lookup)
  expect_identical(build_report_taxa_lookup(lookup, data.frame()), lookup)
})
