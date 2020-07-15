context("test set_role_playing_dimension_name")

test_that("set_role_playing_dimension_name works", {
  d <- st_mrs_age_test$dimension$when
  d <- set_role_playing_dimension_name(d, "test")

  expect_equal(get_role_playing_dimension_name(d), "test")
})
