context("test get_conformed_dimension_names_st")

test_that("get_conformed_dimension_names_st works", {
  ln <- get_conformed_dimension_names_st(ct_mrs_test$star$mrs_age)
  expect_equal(ln, c("when", "when_available", "where"))

  ln <- get_conformed_dimension_names_st(st_mrs_age_test)
  expect_equal(ln, NULL)
})
