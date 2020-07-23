context("test constellation")

test_that("constellation works", {
  ct <- constellation(list(st_mrs_age_test, st_mrs_cause_test), name = "mrs")

  expect_equal(
    lapply(ct$dimension, f <-
             function(x)
               length(x[[1]])),
    list(
      when = 13L,
      when_available = 13L,
      where = 3L
    )
  )
  expect_equal(
    lapply(ct$star$mrs_age$dimension, f <-
             function(x)
               length(x[[1]])),
    list(
      when = 0L,
      when_available = 0L,
      where = 3L,
      who = 5L,
      when_common = 13L
    )
  )
  expect_equal(
    lapply(ct$star$mrs_cause$dimension, f <-
             function(x)
               length(x[[1]])),
    list(
      when = 0L,
      when_received = 0L,
      when_available = 0L,
      where = 3L,
      when_common = 13L
    )
  )
})
