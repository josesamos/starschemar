context("test role_playing_dimension")

test_that("role_playing_dimension works", {
  st <- star_schema(mrs_age_test, dm_mrs_age)
  st <- role_playing_dimension(
    st,
    dim_names = c("when", "when_available"),
    name = "When Common",
    attributes = c("date", "week", "year")
  )

  expect_equal(nrow(st$fact$mrs_age), nrow(mrs_age_test))
  expect_equal(nrow(st$dimension$when), 0)
  expect_equal(nrow(st$dimension$when_available), 0)
  expect_equal(nrow(st$dimension$`When Common`), 9)
})
