context("test run_query")

test_that("run_query works", {
  dq <- dimensional_query(ms_mrs_test) |>
    select_dimension(name = "where",
                    attributes = c("city", "state")) |>
    select_dimension(name = "when",
                    attributes = c("year", "week")) |>
    select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("SUM")
    ) |>
    select_fact(name = "mrs_cause",
               measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
    filter_dimension(name = "when", week <= "03") |>
    filter_dimension(name = "where", city == "Bridgeport") |>
    run_query()

  dim <- list(
    where = structure(
      list(
        where_key = 2L,
        city = "Bridgeport",
        state = "CT"
      ),
      row.names = c(NA,-1L),
      class = c("tbl_df",
                "tbl", "data.frame", "dimension_table"),
      name = "where",
      type = "conformed"
    ),
    when = structure(
      list(
        when_key = 1:3,
        year = c("1962", "1962",
                 "1962"),
        week = c("01", "02", "03")
      ),
      row.names = c(NA,-3L),
      class = c("tbl_df",
                "tbl", "data.frame", "dimension_table"),
      name = "when",
      type = "conformed"
    )
  )

  expect_equal(dq$fact, list(
    mrs_age = structure(
      list(
        when_key = 1:2,
        where_key = c(2L,
                      2L),
        deaths = c(46L, 43L),
        nrow_agg = 3:4,
        mrs_cause_pneumonia_and_influenza_deaths = 3:2,
        mrs_cause_other_deaths = c(43L, 41L),
        mrs_cause_nrow_agg = c(1L,
                               1L)
      ),
      row.names = c(NA,-2L),
      class = c("fact_table", "tbl_df", "tbl", "data.frame"
      ),
      name = "mrs_age",
      foreign_keys = c("when_key",
                       "where_key"),
      measures = c(
        "deaths",
        "nrow_agg",
        "mrs_cause_pneumonia_and_influenza_deaths",
        "mrs_cause_other_deaths",
        "mrs_cause_nrow_agg"
      ),
      agg_functions = c(
        deaths = "SUM",
        nrow_agg = "SUM",
        mrs_cause_pneumonia_and_influenza_deaths = "SUM",
        mrs_cause_other_deaths = "SUM",
        mrs_cause_nrow_agg = "SUM"
      ),
      nrow_agg = "nrow_agg"
    )
  ))
  expect_equal(dq$dimension, dim)

  dq <- dimensional_query(ms_mrs_test) |>
    select_dimension(name = "where",
                    attributes = c("city", "state")) |>
    select_dimension(name = "when",
                    attributes = c("year", "week")) |>
    select_fact(
      name = "mrs_age",
      measures = c("deaths"),
      agg_functions = c("SUM")
    ) |>
    select_fact(name = "mrs_cause",
               measures = c("pneumonia_and_influenza_deaths", "other_deaths")) |>
    filter_dimension(name = "when", week <= "03") |>
    filter_dimension(name = "where", city == "Bridgeport") |>
    run_query(unify_by_grain = FALSE)


  expect_equal(dq$fact, list(
    mrs_age = structure(
      list(
        when_key = 1:2,
        where_key = c(2L,
                      2L),
        deaths = c(46L, 43L),
        nrow_agg = 3:4
      ),
      row.names = 1:2,
      class = c("fact_table", "tbl_df", "tbl", "data.frame"
      ),
      name = "mrs_age",
      foreign_keys = c("when_key",
                       "where_key"),
      measures = c("deaths", "nrow_agg"),
      agg_functions = c(deaths = "SUM",
                        nrow_agg = "SUM"),
      nrow_agg = "nrow_agg"
    ),
    mrs_cause = structure(
      list(
        when_key = 1:2,
        where_key = c(2L, 2L),
        pneumonia_and_influenza_deaths = 3:2,
        other_deaths = c(43L, 41L),
        nrow_agg = c(1L, 1L)
      ),
      row.names = 1:2,
      class = c("fact_table", "tbl_df", "tbl", "data.frame"
      ),
      name = "mrs_cause",
      foreign_keys = c("when_key",
                       "where_key"),
      measures = c("pneumonia_and_influenza_deaths",
                   "other_deaths", "nrow_agg"),
      agg_functions = c(
        pneumonia_and_influenza_deaths = "SUM",
        other_deaths = "SUM",
        nrow_agg = "SUM"
      ),
      nrow_agg = "nrow_agg"
    )
  ))
  expect_equal(dq$dimension, dim)
})
