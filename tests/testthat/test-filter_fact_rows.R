context("test filter_fact_rows")

test_that("filter_fact_rows works", {
  st <- st_mrs_age_test |>
    filter_fact_rows(name = "when", week <= "01") |>
    filter_fact_rows(name = "where", city == "Bridgeport")

  expect_equal(
    st$fact$mrs_age,
    structure(
      list(
        when_available_key = c(3L, 3L, 3L),
        when_key = c(1L,
                     1L, 1L),
        where_key = c(2L, 2L, 2L),
        who_key = c(1L, 4L, 5L),
        deaths = c(3L, 16L, 27L),
        nrow_agg = c(1L, 1L, 1L)
      ),
      row.names = c(NA,-3L),
      class = c("fact_table", "tbl_df", "tbl", "data.frame"),
      name = "mrs_age",
      foreign_keys = c("when_key",
                       "when_available_key", "where_key", "who_key"),
      measures = c("deaths",
                   "nrow_agg"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "nrow_agg"
    )
  )
})
