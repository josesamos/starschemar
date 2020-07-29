context("test snake_case_dimension")

test_that("snake_case_dimension works", {
  st <- star_schema(mrs_age_test, dm_mrs_age)
  d <- snake_case_dimension(st$dimension$when)

  expect_equal(sort(colnames(d)), c("week", "week_ending_date", "when_key", "year"))
})
