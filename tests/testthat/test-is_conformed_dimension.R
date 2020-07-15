context("test is_conformed_dimension")

test_that("is_conformed_dimension works", {

  expect_equal(is_conformed_dimension(ct_mrs_test$dimension$when), TRUE)
  expect_equal(is_conformed_dimension(ct_mrs_test$star$mrs_age$dimension$when), TRUE)
  expect_equal(is_conformed_dimension(ct_mrs_test$star$mrs_age$dimension$who), FALSE)
})
