context("test update_facts_with_dimensions")

test_that("update_facts_with_dimensions works", {
  dimensions <- get_all_dimensions(st_mrs_age_test)
  mod_dim <- update_dimensions(dimensions, updates_st_mrs_age_test)
  st <- update_facts_with_dimensions(st_mrs_age_test, mod_dim)

  expect_equal(
    st$fact$mrs_age,
    structure(
      list(
        who_key = c(
          1L,
          1L,
          1L,
          1L,
          1L,
          2L,
          2L,
          2L,
          3L,
          3L,
          3L,
          3L,
          4L,
          4L,
          4L,
          4L,
          4L,
          4L,
          5L,
          5L,
          5L,
          5L,
          5L,
          5L
        ),
        where_key = c(
          1L,
          1L,
          1L,
          2L,
          2L,
          1L,
          2L,
          2L,
          1L,
          1L,
          2L,
          2L,
          1L,
          1L,
          1L,
          2L,
          2L,
          2L,
          1L,
          1L,
          1L,
          2L,
          2L,
          2L
        ),
        when_available_key = c(
          3L,
          4L,
          7L,
          5L,
          6L,
          7L,
          5L,
          6L,
          4L,
          7L,
          2L,
          5L,
          3L,
          4L,
          7L,
          2L,
          5L,
          6L,
          3L,
          4L,
          7L,
          2L,
          5L,
          6L
        ),
        when_key = c(
          1L,
          3L,
          4L,
          3L,
          4L,
          4L,
          3L,
          4L,
          3L,
          4L,
          1L,
          3L,
          1L,
          3L,
          4L,
          1L,
          3L,
          4L,
          1L,
          3L,
          4L,
          1L,
          3L,
          4L
        ),
        deaths = c(
          3L,
          1L,
          5L,
          4L,
          2L,
          1L,
          1L,
          1L,
          1L,
          3L,
          3L,
          2L,
          16L,
          11L,
          10L,
          9L,
          6L,
          12L,
          27L,
          30L,
          21L,
          38L,
          32L,
          24L
        ),
        nrow_agg = c(
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L,
          1L
        )
      ),
      row.names = c(NA, 24L),
      class = c("tbl_df",
                "tbl", "data.frame", "fact_table"),
      name = "mrs_age",
      foreign_keys = c("who_key",
                       "where_key", "when_available_key", "when_key"),
      measures = c("deaths",
                   "nrow_agg"),
      agg_functions = c("SUM", "SUM"),
      nrow_agg = "nrow_agg"
    )
  )


  st2 <- st_mrs_age_test
  st2$fact$mrs_age <- st2$fact$mrs_age[, -c(1, 2)]
  st2$dimension <- st2$dimension[-c(1, 2, 5)]

  dimensions <- get_all_dimensions(st2)
  mod_dim <- update_dimensions(dimensions, updates_st_mrs_age_test)
  st <- update_facts_with_dimensions(st2, mod_dim)

  expect_equal(
    st$fact$mrs_age,
    structure(
      list(
        who_key = c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 5L,
                    5L),
        where_key = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
        deaths = c(9L,
                   6L, 1L, 2L, 4L, 5L, 37L, 27L, 78L, 94L),
        nrow_agg = c(3L, 2L,
                     1L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)
      ),
      row.names = c(NA, 10L),
      class = c("tbl_df",
                "tbl", "data.frame", "fact_table"),
      name = "mrs_age",
      foreign_keys = c("who_key",
                       "where_key"),
      measures = c("deaths", "nrow_agg"),
      agg_functions = c("SUM",
                        "SUM"),
      nrow_agg = "nrow_agg"
    )
  )
})
