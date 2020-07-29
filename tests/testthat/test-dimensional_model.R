context("test dimensional_model")

test_that("dimensional_model works", {
  sd <- dimensional_model()
  expect_equal(class(sd), "dimensional_model")
})
