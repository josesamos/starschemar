context("test get_role_playing_dimension_name")

test_that("get_role_playing_dimension_name works", {

  expect_equal(get_role_playing_dimension_name(st_mrs_age_test$dimension$when), "when_common")
  expect_equal(get_role_playing_dimension_name(st_mrs_age_test$dimension$who), "")
})
