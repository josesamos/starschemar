context("test get_conformed_dimension")

test_that("get_conformed_dimension works", {
  d <- get_conformed_dimension(ct_mrs_test, "when")

  expect_equal(names(d), c("when_key", "week_ending_date", "week", "year"))
  expect_equal(length(d[[1]]), 13L)
})
