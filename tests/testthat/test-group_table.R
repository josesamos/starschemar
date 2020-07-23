context("test group_table")

test_that("group_table works", {
  ft <- group_table(st_mrs_age_test$fact$mrs_age[, -c(1:3)])

  expect_equal(sort(ft$nrow_agg), c(3L, 4L, 5L, 6L, 6L))
})
