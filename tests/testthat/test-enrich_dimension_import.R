context("test enrich_dimension_import")

test_that("enrich_dimension_import works", {
  tb <-
    enrich_dimension_export(st_mrs_age_test,
                            name = "when_common",
                            attributes = c("week", "year"))
  tb <- tibble::add_column(tb, x = 1, y = 2, z = 3)
  st <-
    enrich_dimension_import(st_mrs_age_test, name = "when_common", tb)

  expect_equal(
    st$dimension$when_common,
    structure(
      list(
        when_common_key = 1:9,
        date = structure(
          c(-2917,-2916,-2910,-2910,-2903,-2898,-2895,-2892, 2932896),
          class = "Date"
        ),
        week = c("01", "01", "02", "Unknown", "03", "04", "04", "05",
                 "03"),
        year = c(
          "1962",
          "1962",
          "1962",
          "Unknown",
          "1962",
          "1962",
          "1962",
          "1962",
          "1962"
        ),
        x = c(1, 1, 1, 1, 1, 1,
              1, 1, 1),
        y = c(2, 2, 2, 2, 2, 2, 2, 2, 2),
        z = c(3, 3, 3,
              3, 3, 3, 3, 3, 3)
      ),
      row.names = c(NA,-9L),
      class = c("tbl_df",
                "tbl", "data.frame", "dimension_table"),
      name = "when_common",
      type = "role_playing"
    )
  )
  expect_equal(
    st$dimension$when,
    structure(
      list(
        when_key = logical(0),
        week_ending_date = logical(0),
        week = logical(0),
        year = logical(0),
        x = character(0),
        y = character(0),
        z = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df",
                "tbl", "data.frame", "dimension_table"),
      name = "when",
      type = "role",
      role_playing = "when_common"
    )
  )
  expect_equal(
    st$dimension$when_available,
    structure(
      list(
        when_available_key = logical(0),
        data_availability_date = logical(0),
        data_availability_week = logical(0),
        data_availability_year = logical(0),
        x = character(0),
        y = character(0),
        z = character(0)
      ),
      row.names = integer(0),
      class = c("tbl_df",
                "tbl", "data.frame", "dimension_table"),
      name = "when_available",
      type = "role",
      role_playing = "when_common"
    )
  )
})
