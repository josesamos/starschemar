context("test set_dimension_name")

test_that("set_dimension_name works", {
  d <- st_mrs_age_test$dimension$when
  d <- set_dimension_name(d, "test")

  expect_equal(get_dimension_name(d), "test")
})
