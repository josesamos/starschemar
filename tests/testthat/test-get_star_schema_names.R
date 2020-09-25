context("test get_star_schema_names")

test_that("get_star_schema_names works", {
  dn <- get_star_schema_names(ct_mrs_test)

  expect_equal(dn, c("mrs_age", "mrs_cause"))
})
