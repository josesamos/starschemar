context("test select_dimension")

test_that("select_dimension works", {
  dq <- dimensional_query(ms_mrs_test) |>
    select_dimension(name = "where",
                    attributes = c("city", "state")) |>
    select_dimension(name = "when")

  expect_equal(dq$dimension$where, c("where_key", "city", "state"))
  expect_equal(dq$dimension$when, c("when_key", "week_ending_date", "week", "year"))
  expect_equal(names(dq$dimension), c("where", "when"))
})
