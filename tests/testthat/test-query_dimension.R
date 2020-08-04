context("test query_dimension")

test_that("query_dimension works", {
  dq <- dimensional_query(ms_mrs_test) %>%
    query_dimension(name = "where",
                    attributes = c("city", "state")) %>%
    query_dimension(name = "when")

  expect_equal(dq$dimension$where, c("where_key", "city", "state"))
  expect_equal(dq$dimension$when, c("when_key", "week_ending_date", "week", "year"))
  expect_equal(names(dq$dimension), c("where", "when"))
})
