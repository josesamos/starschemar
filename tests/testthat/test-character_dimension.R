context("test character_dimension")

test_that("character_dimension works", {
  st <- star_schema(mrs_age_test, dm_mrs_age)
  d <- character_dimension(
    st$dimension$when,
    length_integers = list(WEEK = 2),
    NA_replacement_value = "Unknown"
  )

  expect_equal(sort(d$WEEK), c("01", "02", "03", "Unknown"))
})
