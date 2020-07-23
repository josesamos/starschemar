context("test get_dimension_names")

test_that("get_dimension_names works", {
  dn <- get_dimension_names(st_mrs_age_test)

  expect_equal(dn, c("when", "when_available", "where", "who"))
})
