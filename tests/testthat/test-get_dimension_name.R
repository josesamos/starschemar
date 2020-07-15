context("test get_dimension_name")

test_that("get_dimension_name works", {

  expect_equal(get_dimension_name(st_mrs_age_test$dimension$when), "when")
})
