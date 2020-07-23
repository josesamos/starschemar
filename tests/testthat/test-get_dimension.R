context("test get_dimension")

test_that("get_dimension works", {
  d <- get_dimension(st_mrs_age_test, "when")

  expect_equal(names(d), c("when_key", "week_ending_date", "week", "year"))
  expect_equal(length(d[[1]]), 9L)
})
