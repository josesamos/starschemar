context("test get_conformed_dimension_names")

test_that("get_conformed_dimension_names works", {
  dn <- get_conformed_dimension_names(ct_mrs_test)

  expect_equal(dn, c("when", "when_available", "where"))
})
