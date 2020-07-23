context("test update_dimension")

test_that("update_dimension works", {
  d <-
    update_dimension(st_mrs_age_test$dimension$who, updates_st_mrs_age_test)

  expect_equal(
    d$age_range,
    c(
      "1: <1 year",
      "2: 1-24 years",
      "3: 25-44 years",
      "4: 45-64 years",
      "5: 65+ years"
    )
  )
})
