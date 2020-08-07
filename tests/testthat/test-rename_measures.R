context("test rename_measures")

test_that("rename_measures works", {
  st <-
    st_mrs_age_test %>%
    rename_measures(names = c("deaths", "nrow_agg"),
                    new_names = c("n_deaths", "n_agg"))

  expect_equal(
    attributes(st$fact$mrs_age),
    list(
      names = c(
        "when_available_key",
        "when_key",
        "where_key",
        "who_key",
        "n_deaths",
        "n_agg"
      ),
      row.names = 1:24,
      class = c("tbl_df",
                "tbl", "data.frame", "fact_table"),
      name = "mrs_age",
      foreign_keys = c("when_key",
                       "when_available_key", "where_key", "who_key"),
      measures = c("n_deaths",
                   "n_agg"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "n_agg"
    )
  )
})
