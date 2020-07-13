context("test snake_case")

test_that("snake_case works", {
  st <- star_schema(mrs_age_test, sd_mrs_age)
  st <- snake_case(st)

  expect_equal(
    sort(names(st$fact$mrs_age)),
    c(
      "deaths",
      "nrow_agg",
      "when_available_key",
      "when_key",
      "where_key",
      "who_key"
    )
  )
  expect_equal(sort(names(st$dimension$when)),
               c("week", "week_ending_date", "when_key", "year"))
  expect_equal(
    sort(names(st$dimension$when_available)),
    c(
      "data_availability_date",
      "data_availability_week",
      "data_availability_year",
      "when_available_key"
    )
  )
  expect_equal(sort(names(st$dimension$where)), c("city", "region", "state", "where_key"))
  expect_equal(sort(names(st$dimension$who)), c("age_range", "who_key"))
})
