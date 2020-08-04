context("test dimensional_query")

test_that("dimensional_query works", {
  dq <- dimensional_query(ms_mrs_test)

  expect_equal(names(dq), c("fact", "dimension", "key", "input", "output"))
  expect_equal(names(dq$input), c("fact", "dimension"))
})
