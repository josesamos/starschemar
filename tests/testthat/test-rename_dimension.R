context("test rename_dimension")

test_that("rename_dimension works", {
  st <- st_mrs_age_test |>
    rename_dimension(name = "when", new_name = "when_n")

  expect_equal(
    attributes(st$dimension$when_n),
    list(
      names = c("when_n_key", "week_ending_date", "week", "year"),
      row.names = integer(0),
      name = "when_n",
      type = "role",
      role_playing = "when_common",
      class = c("dimension_table", "tbl_df", "tbl", "data.frame")
    )
  )
  expect_equal(
    attributes(st$fact[[1]]),
    list(
      names = c(
        "when_available_key",
        "when_n_key",
        "where_key",
        "who_key",
        "deaths",
        "nrow_agg"
      ),
      row.names = 1:24,
      name = "mrs_age",
      foreign_keys = c("when_n_key",
                       "when_available_key", "where_key", "who_key"),
      measures = c("deaths",
                   "nrow_agg"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "nrow_agg",
      class = c("fact_table", "tbl_df",
                "tbl", "data.frame")
    )
  )


  st <- st_mrs_age_test |>
    rename_dimension(name = "when_common", new_name = "when_n")

  expect_equal(
    attributes(st$dimension$when_n),
    list(
      names = c("when_n_key", "date", "week", "year"),
      row.names = 1:9,
      name = "when_n",
      type = "role_playing",
      class = c("dimension_table", "tbl_df",
                "tbl", "data.frame")
    )
  )
  expect_equal(
    attributes(st$dimension$when),
    list(
      names = c("when_key", "week_ending_date", "week", "year"),
      row.names = integer(0),
      name = "when",
      type = "role",
      role_playing = "when_n",
      class = c("dimension_table", "tbl_df", "tbl", "data.frame")
    )
  )
  expect_equal(
    attributes(st$dimension$when_available),
    list(
      names = c(
        "when_available_key",
        "data_availability_date",
        "data_availability_week",
        "data_availability_year"
      ),
      row.names = integer(0),
      name = "when_available",
      type = "role",
      role_playing = "when_n",
      class = c("dimension_table", "tbl_df", "tbl", "data.frame")
    )
  )
})
