context("test filter_dimension")

test_that("filter_dimension works", {
  dq <- dimensional_query(ms_mrs_test) |>
    filter_dimension(name = "when", week <= "03") |>
    filter_dimension(name = "where", city == "Bridgeport")

  expect_equal(dq$key$when, c(1L, 2L, 3L, 4L, 6L, 7L, 13L))
  expect_equal(dq$key$where, 2L)
  expect_equal(names(dq$key), c("when", "where"))
})
