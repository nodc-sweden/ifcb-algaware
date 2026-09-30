# Synthetic classifications following the ifcb-classify rule with trained
# thresholds A = 0.5, B = 0.5: class_name is class_auto when score >= its
# threshold, otherwise "unclassified".
make_classifications <- function() {
  data.frame(
    sample_name = c("S1", "S1", "S1", "S1", "S2", "S2"),
    roi_number = 1:6,
    class_name = c("A", "A", "unclassified", "B", "unclassified", "A"),
    class_auto = c("A", "A", "A", "B", "B", "A"),
    score = c(0.9, 0.6, 0.4, 0.8, 0.3, 0.55),
    stringsAsFactors = FALSE
  )
}

trained <- c(A = 0.5, B = 0.5)

# ---- apply_thresholds ----

test_that("apply_thresholds returns input unchanged with no adjustments", {
  cls <- make_classifications()
  expect_identical(apply_thresholds(cls, numeric(0)), cls)
  expect_identical(apply_thresholds(cls, NULL), cls)
})

test_that("apply_thresholds raising a threshold unclassifies low scores", {
  result <- apply_thresholds(make_classifications(), c(A = 0.7))
  expect_equal(result$class_name,
               c("A", "unclassified", "unclassified", "B", "unclassified",
                 "unclassified"))
})

test_that("apply_thresholds lowering a threshold reclaims from unclassified", {
  result <- apply_thresholds(make_classifications(), c(A = 0.3))
  # ROI 3 (auto A, 0.4) returns to A; ROI 5 (auto B, 0.3) is untouched
  expect_equal(result$class_name,
               c("A", "A", "A", "B", "unclassified", "A"))
})

test_that("apply_thresholds keeps a score exactly at the threshold", {
  result <- apply_thresholds(make_classifications(), c(A = 0.6))
  expect_equal(result$class_name[2], "A")
})

test_that("apply_thresholds leaves other classes untouched", {
  cls <- make_classifications()
  result <- apply_thresholds(cls, c(B = 0.99))
  expect_equal(result$class_name[cls$class_auto != "B"],
               cls$class_name[cls$class_auto != "B"])
  expect_equal(result$class_name[4], "unclassified")
})

test_that("apply_thresholds skips rows with missing class_auto", {
  cls <- make_classifications()
  cls$class_auto[2] <- NA_character_
  result <- apply_thresholds(cls, c(A = 0.99))
  expect_equal(result$class_name[2], "A")
})

test_that("apply_thresholds skips rows not following the threshold rule", {
  # A stored label that is neither class_auto nor "unclassified" did not come
  # from the threshold rule, so a threshold change must not overwrite it
  cls <- make_classifications()
  cls$class_name[1] <- "C"
  result <- apply_thresholds(cls, c(A = 0.99))
  expect_equal(result$class_name[1], "C")
})

test_that("apply_thresholds returns input unchanged without class_auto", {
  cls <- make_classifications()
  cls$class_auto <- NULL
  expect_identical(apply_thresholds(cls, c(A = 0.99)), cls)
})

# ---- apply_corrections ----

test_that("apply_corrections relabels matching ROIs", {
  corrections <- data.frame(sample_name = "S1", roi_number = 2L,
                            original_class = "A", new_class = "B",
                            stringsAsFactors = FALSE)
  result <- apply_corrections(make_classifications(), corrections)
  expect_equal(result$class_name[2], "B")
  expect_equal(result$class_name[-2], make_classifications()$class_name[-2])
})

test_that("apply_corrections uses the last correction for a repeated ROI", {
  corrections <- data.frame(sample_name = c("S1", "S1"),
                            roi_number = c(2L, 2L),
                            original_class = c("A", "B"),
                            new_class = c("B", "C"),
                            stringsAsFactors = FALSE)
  result <- apply_corrections(make_classifications(), corrections)
  expect_equal(result$class_name[2], "C")
})

test_that("apply_corrections ignores corrections for unknown ROIs", {
  corrections <- data.frame(sample_name = "S9", roi_number = 1L,
                            original_class = "A", new_class = "B",
                            stringsAsFactors = FALSE)
  cls <- make_classifications()
  expect_equal(apply_corrections(cls, corrections), cls)
})

test_that("apply_corrections returns input unchanged with no corrections", {
  cls <- make_classifications()
  expect_identical(apply_corrections(cls, NULL), cls)
  expect_identical(apply_corrections(cls, data.frame(
    sample_name = character(0), roi_number = integer(0),
    original_class = character(0), new_class = character(0)
  )), cls)
})

# ---- compose_classifications ----

test_that("compose_classifications lets manual corrections override thresholds", {
  corrections <- data.frame(sample_name = "S1", roi_number = 2L,
                            original_class = "A", new_class = "A",
                            stringsAsFactors = FALSE)
  result <- compose_classifications(make_classifications(), c(A = 0.7),
                                    corrections)
  # ROI 2 (score 0.6) would drop below 0.7, but it was confirmed by hand
  expect_equal(result$all$class_name[2], "A")
  expect_equal(result$all$class_name[6], "unclassified")
})

test_that("compose_classifications filters the active slice", {
  result <- compose_classifications(make_classifications(), NULL, NULL,
                                    active_samples = "S2")
  expect_equal(nrow(result$all), 6)
  expect_equal(unique(result$active$sample_name), "S2")
})

test_that("compose_classifications with nothing to apply is identity", {
  cls <- make_classifications()
  result <- compose_classifications(cls, NULL, NULL)
  expect_identical(result$all, cls)
  expect_identical(result$active, cls)
})

# ---- set_threshold_adjustment ----

test_that("set_threshold_adjustment adds an adjustment", {
  result <- set_threshold_adjustment(numeric(0), "A", 0.7, trained)
  expect_equal(result, c(A = 0.7))
})

test_that("set_threshold_adjustment replaces an existing adjustment", {
  result <- set_threshold_adjustment(c(A = 0.7, B = 0.6), "A", 0.8, trained)
  expect_equal(result, c(A = 0.8, B = 0.6))
})

test_that("set_threshold_adjustment drops a value equal to the trained one", {
  result <- set_threshold_adjustment(c(A = 0.7, B = 0.6), "A", 0.5, trained)
  expect_equal(result, c(B = 0.6))
})

test_that("set_threshold_adjustment treats NULL value as reset", {
  result <- set_threshold_adjustment(c(A = 0.7), "A", NULL, trained)
  expect_length(result, 0)
})

test_that("set_threshold_adjustment rejects invalid input", {
  expect_error(set_threshold_adjustment(numeric(0), "Z", 0.7, trained),
               "no trained threshold")
  expect_error(set_threshold_adjustment(numeric(0), "A", 1.5, trained),
               "between 0 and 1")
  expect_error(set_threshold_adjustment(numeric(0), "A", NA_real_, trained),
               "between 0 and 1")
})

# ---- preview_threshold ----

test_that("preview_threshold counts images leaving and joining the class", {
  cls <- make_classifications()
  raised <- preview_threshold(cls, NULL, NULL, "A", 0.7)
  expect_equal(raised, list(n_current = 3L, n_removed = 2L, n_added = 0L))

  lowered <- preview_threshold(cls, NULL, NULL, "A", 0.3)
  expect_equal(lowered, list(n_current = 3L, n_removed = 0L, n_added = 1L))
})

test_that("preview_threshold is relative to existing adjustments", {
  cls <- make_classifications()
  result <- preview_threshold(cls, c(A = 0.7), NULL, "A", 0.95)
  expect_equal(result, list(n_current = 1L, n_removed = 1L, n_added = 0L))
})

test_that("preview_threshold does not count manually corrected ROIs", {
  corrections <- data.frame(sample_name = "S1", roi_number = 2L,
                            original_class = "A", new_class = "A",
                            stringsAsFactors = FALSE)
  result <- preview_threshold(make_classifications(), NULL, corrections,
                              "A", 0.7)
  expect_equal(result$n_removed, 1L)
})

test_that("preview_threshold restricts counts to the given samples", {
  result <- preview_threshold(make_classifications(), NULL, NULL, "A", 0.7,
                              samples = "S2")
  expect_equal(result, list(n_current = 1L, n_removed = 1L, n_added = 0L))
})

# ---- threshold_summary ----

test_that("threshold_summary lists adjusted classes with moved counts", {
  result <- threshold_summary(trained, c(A = 0.7), make_classifications())
  expect_equal(result$class_name, "A")
  expect_equal(result$trained, 0.5)
  expect_equal(result$adjusted, 0.7)
  expect_equal(result$n_moved, 2L)
})

test_that("threshold_summary returns an empty frame with no adjustments", {
  result <- threshold_summary(trained, numeric(0), make_classifications())
  expect_equal(nrow(result), 0)
  expect_equal(names(result),
               c("class_name", "trained", "adjusted", "n_moved"))
})
