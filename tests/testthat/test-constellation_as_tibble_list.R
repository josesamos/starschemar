context("test constellation_as_tibble_list")

test_that("constellation_as_tibble_list works", {
  tl <- constellation_as_tibble_list(ct_mrs_test)
  expect_equal(
    lapply(tl, length),
    list(
      when = 4L,
      when_available = 4L,
      where = 4L,
      mrs_age = 6L,
      who = 2L,
      mrs_cause = 7L,
      when_received = 4L
    )
  )
  expect_equal(
    lapply(tl, f <- function(x)
      length(x[[1]])),
    list(
      when = 13L,
      when_available = 13L,
      where = 3L,
      mrs_age = 24L,
      who = 5L,
      mrs_cause = 6L,
      when_received = 13L
    )
  )

  tl <- constellation_as_tibble_list(ct_mrs_test, TRUE)
  expect_equal(
    lapply(tl, length),
    list(
      when = 4L,
      when_available = 4L,
      where = 4L,
      mrs_age = 6L,
      who = 2L,
      when_common = 4L,
      mrs_cause = 7L,
      when_received = 4L
    )
  )
  expect_equal(
    lapply(tl, f <- function(x)
      length(x[[1]])),
    list(
      when = 13L,
      when_available = 13L,
      where = 3L,
      mrs_age = 24L,
      who = 5L,
      when_common = 13L,
      mrs_cause = 6L,
      when_received = 13L
    )
  )
})
