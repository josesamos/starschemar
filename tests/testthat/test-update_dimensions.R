context("test update_dimensions")

test_that("update_dimensions works", {
  d <-
    update_dimensions(ct_mrs_test$dimension, updates_st_mrs_age_test)

  expect_equal(d$when$week_ending_date,
               structure(
                 c(
                   -2917,-2916,-2915,-2910,-2910,-2905,-2903,-2898,-2897,-2896,-2895,-2892,-2903
                 ),
                 class = "Date"
               ))
  expect_equal(d$where$city, c("Bridgeport", "Bridgeport", "Tacoma"))
})
