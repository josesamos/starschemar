context("test record_update_set")

test_that("record_update_set works", {
  update <- record_update_set()
  expect_equal(class(update), "record_update_set")
})
