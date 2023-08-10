context("test new_fact_table")

test_that("new_fact_table works", {
  ft <-
    new_fact_table(
      st_mrs_age_test$fact$mrs_age,
      "test",
      c("deaths", "nrow_agg"),
      c("SUM", "SUM"),
      c("nrow_agg")
    )

  res <- list(
    names = c(
      "when_available_key",
      "when_key",
      "where_key",
      "who_key",
      "deaths",
      "nrow_agg"
    ),
    row.names = 1:24,
    class = c("fact_table", "tbl_df",
              "tbl", "data.frame"),
    name = "test",
    foreign_keys = c("when_available_key",
                     "when_key", "where_key", "who_key"),
    measures = c("deaths", "nrow_agg"),
    agg_functions = c("SUM", "SUM"),
    nrow_agg = "nrow_agg"
  )

  expect_equal(attributes(ft), res)
})
