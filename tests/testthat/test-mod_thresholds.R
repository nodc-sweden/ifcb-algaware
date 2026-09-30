# Classifications following the ifcb-classify rule with trained thresholds
# A = 0.5, B = 0.5 (same layout as test-thresholds.R). S1 is a Baltic
# sample, S2 a West Coast sample.
make_threshold_rv <- function(corrections = NULL, trained = c(A = 0.5, B = 0.5)) {
  cls <- data.frame(
    sample_name = c("S1", "S1", "S1", "S1", "S2", "S2"),
    roi_number = 1:6,
    class_name = c("A", "A", "unclassified", "B", "unclassified", "A"),
    class_auto = c("A", "A", "A", "B", "B", "A"),
    score = c(0.9, 0.6, 0.4, 0.8, 0.3, 0.55),
    stringsAsFactors = FALSE
  )
  if (is.null(corrections)) {
    corrections <- data.frame(sample_name = character(0),
                              roi_number = integer(0),
                              original_class = character(0),
                              new_class = character(0),
                              stringsAsFactors = FALSE)
  }
  working <- apply_corrections(cls, corrections)
  shiny::reactiveValues(
    data_loaded = TRUE,
    classifications_original = cls,
    classifications_all = working,
    classifications = working,
    corrections = corrections,
    thresholds_trained = trained,
    threshold_adjustments = numeric(0),
    threshold_dimmed = character(0),
    matched_metadata_all = data.frame(pid = c("S1", "S2"),
                                      stringsAsFactors = FALSE),
    excluded_samples = character(0),
    current_region = "EAST",
    baltic_samples = "S1",
    westcoast_samples = "S2",
    current_class_idx = 1L,  # classes in S1: A, B, unclassified
    selected_images = character(0),
    summaries_stale = FALSE
  )
}

# ---- resolve_slider_value ----

test_that("resolve_slider_value snaps a value within half a step", {
  expect_equal(resolve_slider_value(0.72, 0.7224, 0.01), 0.7224)
  expect_equal(resolve_slider_value(0.7274, 0.7224, 0.01), 0.7224)
})

test_that("resolve_slider_value keeps a value further away", {
  expect_equal(resolve_slider_value(0.74, 0.7224, 0.01), 0.74)
})

test_that("resolve_slider_value passes through a missing value", {
  expect_null(resolve_slider_value(NULL, 0.5, 0.01))
})

# ---- mod_thresholds_server ----

test_that("applying a raised threshold unclassifies low scores everywhere", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$setInputs(apply = 1)
    expect_equal(rv$threshold_adjustments, c(A = 0.7))
    # Global: the West Coast sample (ROI 6, 0.55) is affected too
    expect_equal(rv$classifications_all$class_name,
                 c("A", "unclassified", "unclassified", "B", "unclassified",
                   "unclassified"))
    expect_equal(rv$classifications$class_name,
                 rv$classifications_all$class_name)
    expect_true(rv$summaries_stale)
  })
})

test_that("a threshold never overrides a manual correction", {
  corrections <- data.frame(sample_name = "S1", roi_number = 2L,
                            original_class = "A", new_class = "A",
                            stringsAsFactors = FALSE)
  rv <- make_threshold_rv(corrections)
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$setInputs(apply = 1)
    expect_equal(rv$classifications_all$class_name[2], "A")
  })
})

test_that("applying keeps excluded samples out of the active slice", {
  rv <- make_threshold_rv()
  shiny::isolate({
    rv$excluded_samples <- "S2"
    rv$classifications <- rv$classifications_all[1:4, ]
  })
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$setInputs(apply = 1)
    expect_equal(unique(rv$classifications$sample_name), "S1")
    expect_equal(nrow(rv$classifications_all), 6)
  })
})

test_that("resetting a class restores the stored labels", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$setInputs(apply = 1)
    session$setInputs(reset = 1)
    expect_length(rv$threshold_adjustments, 0)
    expect_equal(rv$classifications_all,
                 shiny::isolate(rv$classifications_original))
  })
})

test_that("reset_class resets an adjusted class that is not displayed", {
  rv <- make_threshold_rv()
  shiny::isolate(rv$threshold_adjustments <- c(A = 0.7, B = 0.9))
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(reset_class = "B")
    expect_equal(rv$threshold_adjustments, c(A = 0.7))
  })
})

test_that("reset_all clears every adjustment", {
  rv <- make_threshold_rv()
  shiny::isolate(rv$threshold_adjustments <- c(A = 0.7, B = 0.9))
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(reset_all = 1)
    expect_length(rv$threshold_adjustments, 0)
    expect_equal(rv$classifications_all$class_name,
                 shiny::isolate(rv$classifications_original$class_name))
  })
})

test_that("a slider value at the trained threshold applies nothing", {
  rv <- make_threshold_rv(trained = c(A = 0.5024, B = 0.5))
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.5)
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
    expect_false(rv$summaries_stale)
  })
})

test_that("apply does nothing without trained thresholds", {
  rv <- make_threshold_rv()
  shiny::isolate(rv$thresholds_trained <- NULL)
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
    expect_equal(rv$classifications_all$class_name,
                 shiny::isolate(rv$classifications_original$class_name))
  })
})

test_that("apply does nothing for a class without a trained threshold", {
  rv <- make_threshold_rv()
  shiny::isolate(rv$current_class_idx <- 3L)  # "unclassified"
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
  })
})

test_that("the preview marks the images that would leave the class", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$setInputs(threshold = 0.7)
    session$elapse(1000)
    expect_setequal(rv$threshold_dimmed, c("S1_2", "S2_6"))
    # Nothing is applied by previewing
    expect_length(rv$threshold_adjustments, 0)
  })
})
