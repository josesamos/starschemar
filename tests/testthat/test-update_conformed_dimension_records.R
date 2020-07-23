context("test update_conformed_dimension_records")

test_that("update_conformed_dimension_records works", {
  ct <-
    update_conformed_dimension_records(ct_mrs_test, updates_st_mrs_age_test)

  expect_equal(ct$dimension$when$week_ending_date,
               structure(c(-2917, -2916, -2915, -2910, -2905, -2903, -2898,
                           -2897, -2896, -2895, -2892), class = "Date"))
  expect_equal(ct$dimension$where$city,
               c("Bridgeport", "Tacoma"))
})
