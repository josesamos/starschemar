context("test star_schema_as_flat_table")

test_that("star_schema_as_flat_table works", {
  ft <- star_schema_as_flat_table(st_mrs_age_test)

  expect_equal(sort(st_mrs_age_test$dimension$who$age_range),
               sort(unique(ft$age_range)))
  expect_equal(sort(st_mrs_age_test$fact$mrs_age$deaths),
               sort(ft$deaths))
})
