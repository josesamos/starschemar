context("test get_dimension_attribute_names")

test_that("get_dimension_attribute_names works", {
  names <- st_mrs_age_test |>
    get_dimension_attribute_names("when")

  expect_equal(names, c("week_ending_date", "week", "year"))
})
