context("test star_schema")

test_that("star_schema works", {
  st <- star_schema(mrs_age_test, sd_mrs_age)

  expect_equal(nrow(st$fact$mrs_age), nrow(mrs_age_test))
  expect_equal(nrow(st$dimension$when), 4)
  expect_equal(nrow(st$dimension$when_available), 6)
  expect_equal(nrow(st$dimension$where), 3)
  expect_equal(nrow(st$dimension$who), 5)
})
