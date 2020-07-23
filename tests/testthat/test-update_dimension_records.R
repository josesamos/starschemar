context("test update_dimension_records")

test_that("update_dimension_records works", {
  st <-
    update_dimension_records(st_mrs_age_test, updates_st_mrs_age_test)

  expect_equal(
    st$dimension$who$age_range,
    c(
      "1: <1 year",
      "2: 1-24 years",
      "3: 25-44 years",
      "4: 45-64 years",
      "5: 65+ years"
    )
  )
  expect_equal(st$dimension$when_common$date,
               structure(c(
                 -2917,-2916,-2910,-2903,-2898,-2895,-2892
               ), class = "Date"))
  expect_equal(st$dimension$where$city,
               c("Bridgeport", "Tacoma"))
})
