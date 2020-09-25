context("test get_star_schema")

test_that("get_star_schema works", {
  d <- get_star_schema(ct_mrs_test, "mrs_age")

  expect_equal(class(d), "star_schema")
})
