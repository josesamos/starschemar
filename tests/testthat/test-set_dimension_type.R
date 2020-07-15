context("test set_dimension_type")

test_that("set_dimension_type works", {
  d <- st_mrs_age_test$dimension$when
  d <- set_dimension_type(d, "test")

  expect_equal(get_dimension_type(d), "test")
})
