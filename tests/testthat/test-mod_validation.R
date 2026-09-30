# ---- parse_image_ids ----

test_that("parse_image_ids splits a single image ID correctly", {
  result <- parse_image_ids("D20221023T000155_IFCB134_00042")
  expect_equal(nrow(result), 1L)
  expect_equal(result$sample_name, "D20221023T000155_IFCB134")
  expect_equal(result$roi_number, 42L)
})

test_that("parse_image_ids handles multiple IDs", {
  ids <- c("D20221023T000155_IFCB134_00001",
           "D20221023T000155_IFCB134_00099")
  result <- parse_image_ids(ids)
  expect_equal(nrow(result), 2L)
  expect_equal(result$roi_number, c(1L, 99L))
  expect_true(all(result$sample_name == "D20221023T000155_IFCB134"))
})

test_that("parse_image_ids returns integer roi_number", {
  result <- parse_image_ids("D20221023T000155_IFCB134_00007")
  expect_type(result$roi_number, "integer")
})

# ---- get_region_context ----

make_rv <- function(region = "EAST",
                    baltic  = c("samp1", "samp2"),
                    west    = c("samp3"),
                    classes = c("ClassA", "ClassB", "ClassA"),
                    samples = c("samp1", "samp2", "samp3"),
                    idx     = 1L) {
  rv <- list(
    current_region    = region,
    baltic_samples    = baltic,
    westcoast_samples = west,
    current_class_idx = idx,
    classifications   = data.frame(
      class_name  = classes,
      sample_name = samples,
      stringsAsFactors = FALSE
    )
  )
  rv
}

test_that("get_region_context returns EAST region samples", {
  rv <- make_rv(region = "EAST")
  ctx <- get_region_context(rv)
  expect_equal(ctx$region_samples, c("samp1", "samp2"))
})

test_that("get_region_context returns WEST region samples", {
  rv <- make_rv(region = "WEST")
  ctx <- get_region_context(rv)
  expect_equal(ctx$region_samples, "samp3")
})

test_that("get_region_context returns sorted unique classes for region", {
  # ClassC is on samp3 (WEST), so only ClassA and ClassB should appear for EAST
  rv <- make_rv(region = "EAST",
                classes = c("ClassB", "ClassA", "ClassC"),
                samples = c("samp1", "samp2", "samp3"))
  ctx <- get_region_context(rv)
  expect_equal(ctx$classes, c("ClassA", "ClassB"))
})

test_that("get_region_context sets current_class from idx", {
  rv <- make_rv(region = "EAST",
                classes = c("ClassA", "ClassB"),
                samples = c("samp1", "samp2"),
                idx = 2L)
  ctx <- get_region_context(rv)
  expect_equal(ctx$current_class, "ClassB")
})

test_that("get_region_context clamps idx to number of classes", {
  rv <- make_rv(region = "EAST",
                classes = c("ClassA"),
                samples = c("samp1"),
                idx = 99L)
  ctx <- get_region_context(rv)
  expect_equal(ctx$idx, 1L)
  expect_equal(ctx$current_class, "ClassA")
})

test_that("get_region_context returns NULL current_class when no classes", {
  rv <- make_rv(region = "EAST",
                classes = character(0),
                samples = character(0),
                baltic  = "samp1",
                idx     = 1L)
  ctx <- get_region_context(rv)
  expect_null(ctx$current_class)
})

test_that("importing corrections keeps applied threshold adjustments", {
  original <- data.frame(
    sample_name = c("S1", "S1", "S1"),
    roi_number = 1:3,
    class_name = c("A", "A", "B"),
    class_auto = c("A", "A", "B"),
    score = c(0.9, 0.6, 0.8),
    stringsAsFactors = FALSE
  )
  adjusted <- apply_thresholds(original, c(A = 0.7))
  empty_corrections <- data.frame(sample_name = character(0),
                                  roi_number = integer(0),
                                  original_class = character(0),
                                  new_class = character(0),
                                  stringsAsFactors = FALSE)
  rv <- shiny::reactiveValues(
    data_loaded = TRUE,
    classifications_original = original,
    classifications_all = adjusted,
    classifications = adjusted,
    corrections = empty_corrections,
    custom_classes = NULL,
    class_list = c("A", "B"),
    taxa_lookup = NULL,
    threshold_adjustments = c(A = 0.7),
    matched_metadata_all = data.frame(pid = "S1", stringsAsFactors = FALSE),
    excluded_samples = character(0),
    current_region = "EAST",
    baltic_samples = "S1",
    westcoast_samples = character(0),
    current_class_idx = 1L,
    selected_images = character(0),
    invalidated_classes = character(0),
    summaries_stale = FALSE
  )
  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(data.frame(sample_name = "S1", roi_number = 3L,
                              original_class = "B", new_class = "A"),
                   csv, row.names = FALSE)
  config <- list(local_storage_path = "", annotator = "test", db_folder = "")

  shiny::testServer(mod_validation_server,
                    args = list(rv = rv, config = config), {
    session$setInputs(import_corrections_file = list(datapath = csv,
                                                     name = "c.csv"))
    session$setInputs(confirm_import_corrections = 1)
    # ROI 2 stays unclassified by the threshold; ROI 3 takes the correction
    expect_equal(rv$classifications_all$class_name,
                 c("A", "unclassified", "A"))
    expect_equal(rv$classifications$class_name,
                 c("A", "unclassified", "A"))
  })
})
