context("test constellation_as_multistar")

test_that("constellation_as_multistar works", {
  ms <- constellation_as_multistar(ct_mrs_test)

  expect_equal(sort(ct_mrs_test$dimension$when$week_ending_date),
               sort(ms$dimension$when$week_ending_date))
  expect_equal(sort(ct_mrs_test$star$mrs_age$fact$mrs_age$deaths),
               sort(ms$fact$mrs_age$deaths))
  expect_equal(length(ms$dimension), 5)
})
