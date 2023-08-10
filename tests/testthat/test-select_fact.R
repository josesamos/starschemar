context("test select_fact")

test_that("select_fact works", {
  dq <- dimensional_query(ms_mrs_test) |>
    select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("MAX")
    ) |>
    select_fact(name = "mrs_cause")

  expect_equal(dq$fact$mrs_age, c(deaths = "MAX", nrow_agg = "SUM"))
  expect_equal(dq$fact$mrs_cause, c(nrow_agg = "SUM"))
  expect_equal(names(dq$fact), c("mrs_age", "mrs_cause"))
})
