context("test is_role_playing_dimension")

test_that("is_role_playing_dimension works", {

  expect_equal(is_role_playing_dimension(st_mrs_age_test$dimension$when), FALSE)
  expect_equal(is_role_playing_dimension(st_mrs_age_test$dimension$where), FALSE)
  expect_equal(is_role_playing_dimension(st_mrs_age_test$dimension$when_common), TRUE)
})
