context("test homogenize")

test_that("homogenize works", {
  d <-
    homogenize(st_mrs_age_test$dimension$where,
               attributes = c("a1", "a2", "a3"))

  expect_equal(names(d), c("a1", "a2", "a3"))
})
