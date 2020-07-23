context("test replace_dimension")

test_that("replace_dimension works", {
  st <- replace_dimension(st_mrs_age_test, "when", ct_mrs_test$dimension$when)

  expect_equal(length(st$dimension$when_common[[1]]), 13)
})
