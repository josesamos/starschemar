context("test reference_dimension")

test_that("reference_dimension works", {
  ft <- dereference_dimension(st_mrs_age_test$fact$mrs_age, st_mrs_age_test$dimension$who)
  ft <- reference_dimension(ft, st_mrs_age_test$dimension$who, c("age_range"))

  expect_equal(sort(unique(ft$who_key)), 1:5)
})
