# Enriched microscopy observations for one or more stations, shaped like
# enrich_microscopy_taxa() output after station matching.
mic_obs <- function(station = "Släggö", full = "SLÄGGÖ",
                    coast = "WEST", date = "2026-10-26", chl = NA_real_) {
  data.frame(
    STATION_NAME = full,
    STATION_NAME_SHORT = station,
    COAST = coast,
    visit_date = as.Date(date),
    visit_id = paste0(station, "_", date, "_microscopy"),
    sampler = "HOS (Hose)",
    min_depth_m = 0,
    max_depth_m = 10,
    analysed_by = "A. Analyst",
    name = c("Skeletonema marinoi", "Dinophysis acuminata",
             "Protoperidinium cerasus"),
    display_name = c("Skeletonema marinoi", "Dinophysis acuminata",
                     "Protoperidinium cerasus"),
    trophic_type = c("AU", "MX", "HT"),
    abundance_class = c(5L, 2L, 1L),
    abundance_label = c("50-100%", "Several cells", "Observed"),
    HAB = c(FALSE, TRUE, FALSE),
    italic = TRUE,
    phyto_group = c("Diatoms", "Dinoflagellates", "Dinoflagellates"),
    chl_mean = chl,
    stringsAsFactors = FALSE
  )
}

ifcb_ss <- function(coasts = "EAST") {
  data.frame(
    STATION_NAME_SHORT = paste0("S", seq_along(coasts)),
    COAST = coasts,
    visit_date = as.Date("2026-10-24"),
    visit_id = paste0("S", seq_along(coasts), "_v1"),
    name = "Diatoms",
    AphiaID = 1L,
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 5,
    counts_per_liter = 500,
    chl_mean = 2,
    stringsAsFactors = FALSE
  )
}

# -- format_microscopy_station_for_prompt ---------------------------------------

test_that("microscopy station data lists method, groups and ranked taxa", {
  text <- format_microscopy_station_for_prompt(mic_obs())

  expect_match(text, "Station: Släggö (SLÄGGÖ)", fixed = TRUE)
  expect_match(text, "Region: West Coast", fixed = TRUE)
  expect_match(text, "qualitative light microscopy", fixed = TRUE)
  # No depth, sampler or analyst is sent to the LLM
  expect_false(grepl("0-10 m|Hose|Analyst", text))
  expect_match(text, "Taxa observed: 3", fixed = TRUE)
  expect_match(text, "Diatoms: highest class 5 (50-100%), 1 taxon", fixed = TRUE)
  expect_match(text, "Dinoflagellates: highest class 2 (Several cells), 2 taxa",
               fixed = TRUE)
  expect_match(text, "Dinophysis acuminata [Dinoflagellates; mixotrophic] [HAB]: class 2",
               fixed = TRUE)
  expect_match(text, "Protoperidinium cerasus [Dinoflagellates; heterotrophic]: class 1",
               fixed = TRUE)
  # Highest class first
  expect_lt(regexpr("Skeletonema", text), regexpr("Dinophysis acuminata \\[", text))
  expect_false(grepl("Chl", text))
})

test_that("microscopy station data includes chlorophyll when available", {
  text <- format_microscopy_station_for_prompt(mic_obs(chl = 2.345),
                                               chl_measure = "concentration")
  expect_match(text, "2.35", fixed = TRUE)
})

# -- build_microscopy_station_prompts -------------------------------------------

test_that("microscopy station prompt forbids quantitative claims", {
  prompts <- build_microscopy_station_prompts(mic_obs())
  expect_named(prompts, c("system", "user"))
  expect_match(prompts$system, "WRITING GUIDE", fixed = TRUE)
  expect_match(prompts$user, "light microscopy", fixed = TRUE)
  expect_match(prompts$user, "Do not state or estimate biovolume", fixed = TRUE)
  expect_match(prompts$user, "do not compare", fixed = TRUE)
  expect_match(prompts$user, "5 = dominant, 4 = very common, 3 = common",
               fixed = TRUE)
  expect_false(grepl("Cruise context", prompts$user, fixed = TRUE))
})

test_that("microscopy station prompt compares only with other microscopy stations", {
  all_obs <- rbind(mic_obs(),
                   mic_obs(station = "Å17", full = "Å17")[1:2, ])
  prompts <- build_microscopy_station_prompts(mic_obs(), all_obs)
  expect_match(prompts$user, "Other microscopy stations", fixed = TRUE)
  expect_match(prompts$user, "Å17 (West): 2 taxa", fixed = TRUE)
  expect_match(prompts$user, "Släggö (West): 3 taxa <-- this station",
               fixed = TRUE)
})

# -- format_microscopy_summary_block --------------------------------------------

test_that("microscopy summary block lists stations separately from IFCB", {
  block <- format_microscopy_summary_block(mic_obs(chl = 2))
  expect_match(block, "Microscopy stations", fixed = TRUE)
  expect_match(block, "NOT comparable with the IFCB values", fixed = TRUE)
  expect_match(block, "Släggö (West, 2026-10-26): 3 taxa", fixed = TRUE)
  expect_match(block, "Skeletonema marinoi (class 5)", fixed = TRUE)
  expect_match(block, "HAB: Dinophysis acuminata*", fixed = TRUE)
  expect_match(block, "Groups: Diatoms (highest class 5), Dinoflagellates (highest class 2)",
               fixed = TRUE)
  expect_equal(format_microscopy_summary_block(NULL), "")
  expect_equal(format_microscopy_summary_block(mic_obs()[0, ]), "")
})

test_that("format_cruise_summary_for_prompt appends the microscopy block", {
  text <- format_cruise_summary_for_prompt(ifcb_ss(), microscopy = mic_obs())
  expect_match(text, "S1 (Baltic", fixed = TRUE)
  expect_match(text, "Microscopy stations", fixed = TRUE)
  expect_false(grepl("Microscopy", format_cruise_summary_for_prompt(ifcb_ss())))
})

# -- region_coverage_note with microscopy ---------------------------------------

test_that("region_coverage_note counts microscopy coasts as sampled", {
  note <- region_coverage_note(ifcb_ss("EAST"), mic_obs())
  expect_false(grepl("Do not write a West Coast paragraph", note))
  expect_match(note, "The West Coast stations have no IFCB data", fixed = TRUE)
  expect_match(note, "qualitative light microscopy", fixed = TRUE)
})

test_that("region_coverage_note still restricts when microscopy is on the same coast", {
  note <- region_coverage_note(ifcb_ss("EAST"),
                               mic_obs(station = "BY5", full = "BY5 BORNHOLMSDJ",
                                       coast = "EAST"))
  expect_match(note, "Do not write a West Coast paragraph", fixed = TRUE)
  expect_false(grepl("no IFCB data", note))
})

test_that("region_coverage_note is empty when IFCB covers both coasts", {
  expect_equal(region_coverage_note(ifcb_ss(c("EAST", "WEST")), mic_obs()), "")
})

# -- summary generators ---------------------------------------------------------

test_that("summary prompts include microscopy stations", {
  prompts <- character(0)
  local_mocked_bindings(call_llm = function(system_prompt, user_prompt, ...) {
    prompts <<- c(prompts, user_prompt)
    "ok"
  })
  generate_english_summary(ifcb_ss("EAST"), microscopy = mic_obs())
  generate_swedish_summary(ifcb_ss("EAST"), microscopy = mic_obs())
  expect_length(prompts, 2L)
  expect_true(all(grepl("Microscopy stations", prompts, fixed = TRUE)))
  expect_true(all(grepl("The West Coast stations have no IFCB data", prompts,
                        fixed = TRUE)))
  expect_false(any(grepl("Do not write a West Coast paragraph", prompts)))
})
