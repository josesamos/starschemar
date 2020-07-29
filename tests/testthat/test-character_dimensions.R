context("test character_dimensions")

test_that("character_dimensions works", {
  st <- star_schema(mrs_age_test, dm_mrs_age)
  st <- character_dimensions(
    st,
    length_integers = list(WEEK = 2, `Data Availability Week` = 2),
    NA_replacement_value = "Unknown"
  )

  expect_equal(sort(st$dimension$when$WEEK), c("01", "02", "03", "Unknown"))
  expect_equal(
    sort(st$dimension$when_available$`Data Availability Week`),
    c("01", "02", "03", "04", "04", "05")
  )
  expect_equal(
    sort(st$dimension$when_available$`Data Availability Date`),
    structure(c(-2916, -2910, -2898, -2895, -2892, 2932896), class = "Date")
  )
})
