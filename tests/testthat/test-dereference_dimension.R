context("test dereference_dimension")

test_that("dereference_dimension works", {
  ft <- dereference_dimension(st_mrs_age_test$fact$mrs_age, st_mrs_age_test$dimension$who)

  values <- unique(ft$age_range)
  expect_equal(length(values), 5)
  for (v in values) {
    expect_equal(v %in% c("<1 year", "1-24 years", "25-44 years", "45-64 years", "65+ years"), TRUE)
  }
})
