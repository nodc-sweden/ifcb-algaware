make_io_corrections <- function(n = 2) {
  data.frame(
    sample_name = rep("S1", n),
    roi_number = seq_len(n),
    original_class = rep("A", n),
    new_class = rep("B", n),
    stringsAsFactors = FALSE
  )
}

io_thresholds <- data.frame(class_name = c("A", "B"), trained = c(0.5, 0.4),
                            adjusted = c(0.7, 0.2), n_moved = c(3L, 1L),
                            stringsAsFactors = FALSE)

io_trained <- c(A = 0.5, B = 0.4, C = 0.9)

# Write an export to CSV and read it back as the import handler does
roundtrip_csv <- function(df) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# ---- enrich_corrections_for_export ----

test_that("enrich_corrections_for_export handles an empty corrections log", {
  result <- enrich_corrections_for_export(make_io_corrections(0), NULL)
  expect_equal(nrow(result), 0)
  expect_true("custom_is_diatom" %in% names(result))
})

# ---- build_corrections_export ----

test_that("build_corrections_export marks correction rows", {
  result <- build_corrections_export(make_io_corrections(), NULL)
  expect_equal(result$record_type, c("correction", "correction"))
  expect_true(all(is.na(result$threshold_class)))
  expect_equal(names(result)[1:4], c("sample_name", "roi_number",
                                     "original_class", "new_class"))
})

test_that("build_corrections_export appends one row per adjusted class", {
  result <- build_corrections_export(make_io_corrections(), NULL,
                                     io_thresholds)
  expect_equal(nrow(result), 4)
  thr <- result[result$record_type == "threshold", ]
  expect_equal(thr$threshold_class, c("A", "B"))
  expect_equal(thr$threshold_trained, c(0.5, 0.4))
  expect_equal(thr$threshold_adjusted, c(0.7, 0.2))
  expect_equal(thr$threshold_n_moved, c(3L, 1L))
  expect_true(all(is.na(thr$sample_name)))
  expect_true(all(is.na(thr$roi_number)))
})

test_that("build_corrections_export exports thresholds without corrections", {
  result <- build_corrections_export(make_io_corrections(0), NULL,
                                     io_thresholds)
  expect_equal(nrow(result), 2)
  expect_equal(result$record_type, c("threshold", "threshold"))
})

test_that("build_corrections_export keeps custom class metadata", {
  corrections <- make_io_corrections(1)
  corrections$new_class <- "Custom"
  custom <- data.frame(clean_names = "Custom", name = "Custom sp.",
                       sflag = "", AphiaID = 1L, HAB = FALSE, italic = TRUE,
                       is_diatom = FALSE, stringsAsFactors = FALSE)
  result <- build_corrections_export(corrections, custom, io_thresholds)
  expect_equal(result$custom_sci_name[1], "Custom sp.")
  expect_true(all(is.na(result$custom_sci_name[2:3])))
})

# ---- split_corrections_import ----

test_that("split_corrections_import treats files without record_type as corrections", {
  df <- make_io_corrections()
  result <- split_corrections_import(df)
  expect_equal(result$corrections, df)
  expect_equal(nrow(result$thresholds), 0)
})

test_that("split_corrections_import separates threshold rows after a CSV roundtrip", {
  df <- roundtrip_csv(build_corrections_export(make_io_corrections(), NULL,
                                               io_thresholds))
  result <- split_corrections_import(df)
  expect_equal(nrow(result$corrections), 2)
  expect_equal(result$corrections$new_class, c("B", "B"))
  expect_equal(result$thresholds$threshold_class, c("A", "B"))
})

# ---- adjustments_from_import ----

test_that("adjustments_from_import restores valid adjustments", {
  thr <- split_corrections_import(roundtrip_csv(
    build_corrections_export(make_io_corrections(0), NULL, io_thresholds)
  ))$thresholds
  result <- adjustments_from_import(thr, io_trained)
  expect_equal(result$adjustments, c(A = 0.7, B = 0.2))
  expect_equal(nrow(result$skipped), 0)
})

test_that("adjustments_from_import skips classes unknown to the classifier", {
  thr <- data.frame(threshold_class = c("A", "Z"), threshold_trained = c(0.5, 0.3),
                    threshold_adjusted = c(0.7, 0.6), stringsAsFactors = FALSE)
  result <- adjustments_from_import(thr, io_trained)
  expect_equal(result$adjustments, c(A = 0.7))
  expect_equal(result$skipped$class_name, "Z")
})

test_that("adjustments_from_import skips a different trained threshold", {
  thr <- data.frame(threshold_class = "A", threshold_trained = 0.45,
                    threshold_adjusted = 0.7, stringsAsFactors = FALSE)
  result <- adjustments_from_import(thr, io_trained)
  expect_length(result$adjustments, 0)
  expect_match(result$skipped$reason, "different classifier")
})

test_that("adjustments_from_import skips invalid adjusted values", {
  thr <- data.frame(threshold_class = c("A", "B"), threshold_trained = c(0.5, 0.4),
                    threshold_adjusted = c(1.5, NA), stringsAsFactors = FALSE)
  result <- adjustments_from_import(thr, io_trained)
  expect_length(result$adjustments, 0)
  expect_equal(result$skipped$class_name, c("A", "B"))
})

test_that("adjustments_from_import skips everything without trained thresholds", {
  thr <- data.frame(threshold_class = "A", threshold_trained = 0.5,
                    threshold_adjusted = 0.7, stringsAsFactors = FALSE)
  result <- adjustments_from_import(thr, NULL)
  expect_length(result$adjustments, 0)
  expect_match(result$skipped$reason, "not available")
})

test_that("adjustments_from_import returns nothing for no rows", {
  result <- adjustments_from_import(NULL, io_trained)
  expect_length(result$adjustments, 0)
  expect_equal(nrow(result$skipped), 0)
})

# ---- format_threshold_adjustments ----

test_that("format_threshold_adjustments lists classes with both thresholds", {
  expect_equal(format_threshold_adjustments(io_thresholds),
               "A (0.50 → 0.70); B (0.40 → 0.20)")
})

test_that("format_threshold_adjustments returns NULL with no adjustments", {
  expect_null(format_threshold_adjustments(NULL))
  expect_null(format_threshold_adjustments(io_thresholds[0, ]))
})
