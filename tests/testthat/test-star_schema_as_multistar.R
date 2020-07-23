context("test star_schema_as_multistar")

test_that("star_schema_as_multistar works", {
  ms <- star_schema_as_multistar(st_mrs_age_test)

  expect_equal(sort(st_mrs_age_test$dimension$who$age_range),
               sort(ms$dimension$who$age_range))
  expect_equal(sort(st_mrs_age_test$fact$mrs_age$deaths),
               sort(ms$fact$mrs_age$deaths))
  expect_equal(length(ms$dimension), 4)
})
