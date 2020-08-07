context("test get_measure_names")

test_that("get_measure_names works", {
  names <- st_mrs_age_test %>% get_measure_names()

  expect_equal(names, c("deaths", "nrow_agg"))
})
