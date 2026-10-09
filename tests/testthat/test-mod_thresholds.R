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

# Report a value from the slider on screen. Its input id changes with every
# rendering (see mod_thresholds_server), so it is taken from slider().
move_slider <- function(session, id, value) {
  do.call(session$setInputs, stats::setNames(list(value), id))
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
    move_slider(session, slider()$id, 0.7)
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
    move_slider(session, slider()$id, 0.7)
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
    move_slider(session, slider()$id, 0.7)
    session$setInputs(apply = 1)
    expect_equal(unique(rv$classifications$sample_name), "S1")
    expect_equal(nrow(rv$classifications_all), 6)
  })
})

test_that("resetting a class restores the stored labels", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.7)
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

test_that("resetting an emptied class keeps the class being viewed", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.95)
    session$setInputs(apply = 1)  # empties A in S1
    expect_equal(get_region_context(rv)$classes, c("B", "unclassified"))
    rv$current_class_idx <- 2L
    session$flushReact()
    expect_equal(get_region_context(rv)$current_class, "unclassified")

    # A returns to the front of the class list, shifting the other classes
    session$setInputs(reset_class = "A")
    expect_equal(get_region_context(rv)$classes, c("A", "B", "unclassified"))
    expect_equal(get_region_context(rv)$current_class, "unclassified")
  })
})

test_that("emptying the viewed class moves on to the class taking its place", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.95)
    session$setInputs(apply = 1)
    expect_equal(rv$current_class_idx, 1L)
    expect_equal(get_region_context(rv)$current_class, "B")
  })
})

test_that("restore_current_class follows the class by name", {
  rv <- shiny::reactiveValues(
    classifications = data.frame(sample_name = "S1",
                                 class_name = c("A", "B", "C"),
                                 stringsAsFactors = FALSE),
    current_region = "EAST", baltic_samples = "S1",
    westcoast_samples = character(0), current_class_idx = 1L
  )
  shiny::isolate({
    restore_current_class(rv, "C")
    expect_equal(rv$current_class_idx, 3L)
    # A class that is gone, or none at all: the index is only clamped
    rv$current_class_idx <- 5L
    restore_current_class(rv, "Z")
    expect_equal(rv$current_class_idx, 3L)
    restore_current_class(rv, NULL)
    expect_equal(rv$current_class_idx, 3L)
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
    move_slider(session, slider()$id, 0.5)
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
    expect_false(rv$summaries_stale)
  })
})

test_that("apply does nothing without trained thresholds", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.7)
    rv$thresholds_trained <- NULL
    session$flushReact()
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
    expect_equal(rv$classifications_all$class_name,
                 shiny::isolate(rv$classifications_original$class_name))
  })
})

test_that("apply does nothing for a class without a trained threshold", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.7)
    rv$current_class_idx <- 3L  # "unclassified"
    session$flushReact()
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
  })
})

test_that("the preview marks the images that would leave the class", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.7)
    session$elapse(1000)
    expect_setequal(rv$threshold_dimmed, c("S1_2", "S2_6"))
    # Nothing is applied by previewing
    expect_length(rv$threshold_adjustments, 0)
  })
})

test_that("a burst of slider values computes a single preview", {
  # While dragging, the slider reports many values; only the one it settles
  # on may be computed, or the server falls behind on a large cruise
  calls <- 0L
  real_preview <- preview_from_context
  testthat::local_mocked_bindings(
    preview_from_context = function(...) {
      calls <<- calls + 1L
      real_preview(...)
    }
  )
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    # Consume the initial flush, which in the app happens before the slider
    # exists (otherwise the first value counts as the debouncer's start value)
    session$flushReact()
    move_slider(session, slider()$id, 0.6)
    move_slider(session, slider()$id, 0.9)
    move_slider(session, slider()$id, 0.7)
    session$elapse(1000)
    expect_equal(calls, 1L)
    expect_setequal(rv$threshold_dimmed, c("S1_2", "S2_6"))
  })
})

# ---- a slider value belongs to the slider that reported it ----

count_preview_contexts <- function(env = parent.frame()) {
  calls <- 0L
  real_context <- preview_context
  testthat::local_mocked_bindings(
    preview_context = function(...) {
      calls <<- calls + 1L
      real_context(...)
    },
    .env = env
  )
  function() calls
}

test_that("changing class leaves no preview from the previous slider", {
  n_contexts <- count_preview_contexts()
  rv <- make_threshold_rv(trained = c(A = 0.9, B = 0.5))
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    session$flushReact()
    move_slider(session, slider()$id, 0.9)  # A's slider reports its position
    session$elapse(1000)

    # In the app the slider of the new class only reports back after a round
    # trip; until then A's 0.9 must not be read as a threshold for B
    rv$current_class_idx <- 2L
    session$flushReact()
    session$elapse(1000)
    expect_equal(rv$threshold_dimmed, character(0))
    expect_null(output$preview)
    expect_equal(n_contexts(), 0L)

    # B's slider reports the threshold in effect: still nothing to preview
    move_slider(session, slider()$id, 0.5)
    session$elapse(1000)
    expect_equal(n_contexts(), 0L)

    # ...until it is moved
    move_slider(session, slider()$id, 0.85)
    session$elapse(1000)
    expect_equal(rv$threshold_dimmed, "S1_4")
    expect_equal(n_contexts(), 1L)
  })
})

test_that("resetting a class leaves no preview from the previous slider", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.7)
    session$setInputs(apply = 1)
    session$elapse(1000)

    # The slider that reported 0.7 is replaced by one at the trained 0.5
    session$setInputs(reset = 1)
    session$elapse(1000)
    expect_length(rv$threshold_adjustments, 0)
    expect_equal(rv$threshold_dimmed, character(0))
    expect_null(output$preview)
  })
})

test_that("every rendering of the slider gets an input id of its own", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    first <- slider()$id
    expect_equal(slider()$id, first)
    rv$current_class_idx <- 2L
    session$flushReact()
    second <- slider()$id
    rv$current_class_idx <- 1L
    session$flushReact()
    expect_length(unique(c(first, second, slider()$id)), 3)
    expect_match(as.character(output$controls$html), slider()$id,
                 fixed = TRUE)
  })
})

test_that("apply ignores a value from a slider no longer on screen", {
  rv <- make_threshold_rv()
  shiny::testServer(mod_thresholds_server, args = list(rv = rv), {
    move_slider(session, slider()$id, 0.95)  # at class A
    rv$current_class_idx <- 2L               # B, whose slider has not reported
    session$flushReact()
    session$setInputs(apply = 1)
    expect_length(rv$threshold_adjustments, 0)
  })
})
