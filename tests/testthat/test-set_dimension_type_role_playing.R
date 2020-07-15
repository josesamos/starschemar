context("test set_role_playing_dimension_type")

test_that("set_role_playing_dimension_type works", {
  d <- st_mrs_age_test$dimension$when
  d <- set_dimension_type_role_playing(d)

  expect_equal(is_role_playing_dimension(d), TRUE)
})
