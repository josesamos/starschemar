context("test set_dimension_type_conformed")

test_that("set_dimension_type_conformed works", {
  d <- st_mrs_age_test$dimension$when
  d <- set_dimension_type_conformed(d)

  expect_equal(is_conformed_dimension(d), TRUE)
})
