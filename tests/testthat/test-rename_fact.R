context("test rename_fact")

test_that("rename_fact works", {
  st <- st_mrs_age_test |> rename_fact("age")

  expect_equal(
    attributes(st$fact[[1]]),
    list(
      names = c(
        "when_available_key",
        "when_key",
        "where_key",
        "who_key",
        "deaths",
        "nrow_agg"
      ),
      row.names = 1:24,
      class = c("tbl_df",
                "tbl", "data.frame", "fact_table"),
      name = "age",
      foreign_keys = c("when_key",
                       "when_available_key", "where_key", "who_key"),
      measures = c("deaths",
                   "nrow_agg"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "nrow_agg"
    )
  )
})
