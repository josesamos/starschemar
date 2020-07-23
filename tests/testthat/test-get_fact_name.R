context("test get_fact_name")

test_that("get_fact_name works", {

  expect_equal(get_fact_name(st_mrs_age_test), "mrs_age")
})
