context("test purge_dimensions")

test_that("purge_dimensions works", {
  st <- st_mrs_age_test %>%
    filter_fact_rows(name = "when", week <= "01") %>%
    filter_fact_rows(name = "where", city == "Bridgeport") %>%
    purge_dimensions()

  expect_equal(
    st$dimension$where,
    structure(
      list(
        where_key = 2L,
        region = "1",
        state = "CT",
        city = "Bridgeport"
      ),
      row.names = c(NA,-1L),
      class = c("tbl_df", "tbl", "data.frame", "dimension_table"),
      name = "where",
      type = "general"
    )
  )

  expect_equal(
    st$dimension$who,
    structure(
      list(
        who_key = c(1L, 4L, 5L),
        age_range = c("<1 year",
                      "45-64 years", "65+ years")
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df",
                "tbl", "data.frame", "dimension_table"),
      name = "who",
      type = "general"
    )
  )

  expect_equal(
    st$dimension$when_common,
    structure(
      list(
        when_common_key = c(1L, 3L),
        date = structure(c(-2917, -2910), class = "Date"),
        week = c("01", "02"),
        year = c("1962",
                 "1962")
      ),
      row.names = c(NA, -2L),
      class = c("tbl_df", "tbl",
                "data.frame", "dimension_table"),
      name = "when_common",
      type = "role_playing"
    )
  )
})
