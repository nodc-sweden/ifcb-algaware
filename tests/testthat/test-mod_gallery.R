make_imgs <- function(n) {
  data.frame(id = seq_len(n), stringsAsFactors = FALSE)
}

test_that("paginate_images returns first page correctly", {
  imgs <- make_imgs(25)
  result <- paginate_images(imgs, page = 1L, page_size = 10L)
  expect_equal(nrow(result), 10L)
  expect_equal(result$id, 1:10)
})

test_that("paginate_images returns last page with fewer rows", {
  imgs <- make_imgs(25)
  result <- paginate_images(imgs, page = 3L, page_size = 10L)
  expect_equal(nrow(result), 5L)
  expect_equal(result$id, 21:25)
})

test_that("paginate_images clamps page to last valid page", {
  imgs <- make_imgs(15)
  result <- paginate_images(imgs, page = 99L, page_size = 10L)
  expect_equal(nrow(result), 5L)
  expect_equal(result$id, 11:15)
})

test_that("paginate_images returns all rows when page_size >= nrow", {
  imgs <- make_imgs(8)
  result <- paginate_images(imgs, page = 1L, page_size = 100L)
  expect_equal(nrow(result), 8L)
})

test_that("paginate_images returns second page correctly", {
  imgs <- make_imgs(30)
  result <- paginate_images(imgs, page = 2L, page_size = 10L)
  expect_equal(result$id, 11:20)
})

# ---- match_pending_echo (class selectize echo guard) ----

test_that("match_pending_echo treats a value not in the queue as user action", {
  res <- match_pending_echo(c("A", "B"), "C")
  expect_false(res$is_echo)
  expect_equal(res$queue, c("A", "B"))
})

test_that("match_pending_echo swallows an in-order echo and drops it", {
  res <- match_pending_echo(c("A", "B", "C"), "A")
  expect_true(res$is_echo)
  expect_equal(res$queue, c("B", "C"))
})

test_that("match_pending_echo drains through the last match on a coalesced echo", {
  # Rapid navigation: A and B were pushed but coalesced; only C echoes back.
  res <- match_pending_echo(c("A", "B", "C"), "C")
  expect_true(res$is_echo)
  expect_equal(res$queue, character(0))
})

test_that("match_pending_echo handles duplicate values by clearing through the last", {
  res <- match_pending_echo(c("A", "B", "A"), "A")
  expect_true(res$is_echo)
  expect_equal(res$queue, character(0))
})

test_that("match_pending_echo handles an empty queue and NA selection", {
  expect_false(match_pending_echo(character(0), "A")$is_echo)
  expect_false(match_pending_echo(c("A", "B"), NA_character_)$is_echo)
  expect_true(match_pending_echo(c(NA_character_, "B"), NA_character_)$is_echo)
})
