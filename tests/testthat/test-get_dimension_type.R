context("test get_dimension_type")

test_that("get_dimension_type works", {

  expect_equal(get_dimension_type(st_mrs_age_test$dimension$when), "role")
})
