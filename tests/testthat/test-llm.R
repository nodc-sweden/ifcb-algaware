test_that("strip_markdown removes bold markers", {
  result <- algaware:::strip_markdown("This is **bold** text")
  expect_equal(result, "This is bold text")
})

test_that("strip_markdown removes italic markers", {
  result <- algaware:::strip_markdown("This is *italic* text")
  expect_equal(result, "This is italic text")
})

test_that("strip_markdown preserves HAB asterisks after species names", {
  result <- algaware:::strip_markdown("Alexandrium catenella* was observed")
  expect_equal(result, "Alexandrium catenella* was observed")
})

test_that("strip_markdown handles mixed formatting", {
  result <- algaware:::strip_markdown("**Bold** and *italic* with species*")
  expect_equal(result, "Bold and italic with species*")
})

test_that("strip_markdown trims whitespace", {
  result <- algaware:::strip_markdown("  hello  ")
  expect_equal(result, "hello")
})

test_that("format_report_paragraph returns fpar with no taxa_lookup", {
  result <- algaware:::format_report_paragraph("Hello world")
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph returns fpar with empty taxa_lookup", {
  taxa <- data.frame(name = character(0), italic = logical(0),
                     stringsAsFactors = FALSE)
  result <- algaware:::format_report_paragraph("Hello world", taxa)
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph italicizes species names", {
  taxa <- data.frame(
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    italic = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "Skeletonema marinoi was dominant.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_station_data_for_prompt produces expected output", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY31",
    STATION_NAME = "BY31 Landsort Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Skeletonema marinoi", "Dinophysis acuminata"),
    biovolume_mm3_per_liter = c(0.5, 0.1),
    carbon_ug_per_liter = c(10.0, 2.0),
    counts_per_liter = c(1000, 200),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data)
  expect_type(result, "character")
  expect_true(grepl("BY31", result))
  expect_true(grepl("Baltic Sea", result))
  expect_true(grepl("Skeletonema marinoi", result))
})

test_that("llm_available returns logical", {
  result <- llm_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("load_writing_guide returns non-empty string", {
  result <- algaware:::load_writing_guide()
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("add_formatted_par adds paragraphs to document", {
  doc <- officer::read_docx()
  result <- algaware:::add_formatted_par(doc, "Hello world")
  expect_s3_class(result, "rdocx")
})

test_that("add_formatted_par splits on double newlines", {
  doc <- officer::read_docx()
  result <- algaware:::add_formatted_par(doc, "Para one.\n\nPara two.")
  expect_s3_class(result, "rdocx")
})

test_that("add_formatted_par applies taxa formatting", {
  doc <- officer::read_docx()
  taxa <- data.frame(
    name = "Skeletonema marinoi",
    italic = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::add_formatted_par(
    doc, "Skeletonema marinoi was dominant.", taxa
  )
  expect_s3_class(result, "rdocx")
})

test_that("format_report_paragraph handles HAB asterisks", {
  taxa <- data.frame(
    name = c("Alexandrium catenella"),
    italic = c(TRUE),
    HAB = c(TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "Alexandrium catenella* was detected.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_report_paragraph handles abbreviated species", {
  taxa <- data.frame(
    name = c("Skeletonema marinoi"),
    italic = c(TRUE),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_report_paragraph(
    "S. marinoi was abundant.", taxa
  )
  expect_s3_class(result, "fpar")
})

test_that("format_cruise_summary_for_prompt produces text", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "BY5", "ANHOLT", "ANHOLT"),
    COAST = c("EAST", "EAST", "WEST", "WEST"),
    visit_date = as.Date(c("2024-03-15", "2024-03-15",
                            "2024-03-16", "2024-03-16")),
    visit_id = c("BY5_visit1", "BY5_visit1", "ANHOLT_visit1", "ANHOLT_visit1"),
    name = c("Taxon A", "Taxon B", "Taxon C", "Taxon D"),
    biovolume_mm3_per_liter = c(0.5, 0.1, 0.3, 0.05),
    carbon_ug_per_liter = c(10, 2, 6, 1),
    counts_per_liter = c(1000, 200, 500, 100),
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(station_summary)
  expect_type(result, "character")
  expect_true(grepl("BY5", result))
  expect_true(grepl("ANHOLT", result))
})

test_that("format_cruise_summary_for_prompt marks HAB species", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_visit1",
    name = "Alexandrium catenella",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Alexandrium catenella",
    HAB = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(
    station_summary, taxa
  )
  expect_true(grepl("HAB", result))
})

test_that("format_station_data_for_prompt handles HAB taxa", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY31",
    STATION_NAME = "BY31 Landsort Deep",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = c("Alexandrium catenella", "Skeletonema marinoi"),
    biovolume_mm3_per_liter = c(0.1, 0.5),
    carbon_ug_per_liter = c(2, 10),
    counts_per_liter = c(200, 1000),
    stringsAsFactors = FALSE
  )
  taxa <- data.frame(
    name = "Alexandrium catenella",
    HAB = TRUE,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data, taxa)
  expect_true(grepl("\\[HAB\\]", result))
})

test_that("format_station_data_for_prompt includes chl when present", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    chl_mean = 3.14,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(station_data)
  expect_true(grepl("3.14", result))
})

test_that("format_station_data_for_prompt includes unclassified note when >80%", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data, unclassified_pct = 85
  )
  expect_true(grepl("85%", result))
  expect_true(grepl("unclassified", result))
})

test_that("format_station_data_for_prompt omits unclassified note when <=80%", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data, unclassified_pct = 50
  )
  expect_false(grepl("unclassified", result))
})

test_that("format_station_data_for_prompt omits note when pct is NULL", {
  station_data <- data.frame(
    STATION_NAME_SHORT = "BY5",
    STATION_NAME = "BY5 Bornholmsdjupet",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_station_data_for_prompt(
    station_data, unclassified_pct = NULL
  )
  expect_false(grepl("unclassified", result))
})

test_that("format_cruise_summary_for_prompt includes unclassified tag when >80%", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = c("BY5", "ANHOLT"),
    COAST = c("EAST", "WEST"),
    visit_date = as.Date(c("2024-03-15", "2024-03-16")),
    visit_id = c("BY5_visit1", "ANHOLT_visit1"),
    name = c("Taxon A", "Taxon B"),
    biovolume_mm3_per_liter = c(0.5, 0.3),
    carbon_ug_per_liter = c(10, 6),
    counts_per_liter = c(1000, 500),
    stringsAsFactors = FALSE
  )
  fractions <- list(BY5_visit1 = 90, ANHOLT_visit1 = 40)
  result <- algaware:::format_cruise_summary_for_prompt(
    station_summary, unclassified_fractions = fractions
  )
  # BY5 has 90% -> should show UNCLASSIFIED tag
  expect_true(grepl("UNCLASSIFIED: 90%", result))
  # ANHOLT has 40% -> should NOT show tag
  expect_false(grepl("UNCLASSIFIED: 40%", result))
})

test_that("format_cruise_summary_for_prompt works without unclassified_fractions", {
  station_summary <- data.frame(
    STATION_NAME_SHORT = "BY5",
    COAST = "EAST",
    visit_date = as.Date("2024-03-15"),
    visit_id = "BY5_visit1",
    name = "Taxon A",
    biovolume_mm3_per_liter = 0.5,
    carbon_ug_per_liter = 10,
    counts_per_liter = 1000,
    stringsAsFactors = FALSE
  )
  result <- algaware:::format_cruise_summary_for_prompt(station_summary)
  expect_type(result, "character")
  expect_false(grepl("UNCLASSIFIED", result))
})

test_that("call_openai errors without API key", {
  withr::with_envvar(c(OPENAI_API_KEY = ""), {
    expect_error(
      algaware:::call_openai("system", "user"),
      "OPENAI_API_KEY"
    )
  })
})

test_that("call_gemini errors without API key", {
  withr::with_envvar(c(GEMINI_API_KEY = ""), {
    expect_error(
      algaware:::call_gemini("system", "user"),
      "GEMINI_API_KEY"
    )
  })
})

test_that("llm_provider returns correct provider", {
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test", GEMINI_API_KEY = ""), {
    expect_equal(llm_provider(), "openai")
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = "AIza-test"), {
    expect_equal(llm_provider(), "gemini")
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = ""), {
    expect_equal(llm_provider(), "none")
  })
})

test_that("llm_provider prefers OpenAI when both keys set", {
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test", GEMINI_API_KEY = "AIza-test"), {
    expect_equal(llm_provider(), "openai")
  })
})

test_that("llm_available detects Gemini key", {
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = "AIza-test"), {
    expect_true(llm_available())
  })
})

test_that("call_llm errors with no provider", {
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = ""), {
    expect_error(
      algaware:::call_llm("system", "user"),
      "No LLM API key configured"
    )
  })
})

test_that("llm_providers returns all available providers", {
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test", GEMINI_API_KEY = "AIza-test"), {
    providers <- llm_providers()
    expect_equal(providers, c("openai", "gemini"))
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = "AIza-test"), {
    expect_equal(llm_providers(), "gemini")
  })
  withr::with_envvar(c(OPENAI_API_KEY = "", GEMINI_API_KEY = ""), {
    expect_equal(llm_providers(), character(0))
  })
})

test_that("llm_model_name returns correct default per provider", {
  withr::with_envvar(c(OPENAI_MODEL = "", GEMINI_MODEL = ""), {
    expect_equal(llm_model_name("openai"), "gpt-4.1")
    expect_equal(llm_model_name("gemini"), "gemini-2.5-flash")
  })
})

test_that("llm_model_name respects env var override", {
  withr::with_envvar(c(OPENAI_MODEL = "gpt-5"), {
    expect_equal(llm_model_name("openai"), "gpt-5")
  })
  withr::with_envvar(c(GEMINI_MODEL = "gemini-3.0-flash"), {
    expect_equal(llm_model_name("gemini"), "gemini-3.0-flash")
  })
})
