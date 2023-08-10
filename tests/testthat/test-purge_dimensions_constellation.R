context("test purge_dimensions_constellation")

test_that("purge_dimensions_constellation works", {
  st1 <- ct_mrs_test$star$mrs_age %>%
    filter_fact_rows(name = "where", city == "Bridgepor")

  st2 <- ct_mrs_test$star$mrs_cause %>%
    filter_fact_rows(name = "where", city == "Bridgepor")

  ct <- ct_mrs_test %>%
    incremental_refresh_constellation(st1, existing = "delete") %>%
    incremental_refresh_constellation(st2, existing = "delete") %>%
    purge_dimensions_constellation()

  expect_equal(
    ct$dimension$where,
    structure(
      list(
        where_key = 1:2,
        region = c("1", "9"),
        state = c("CT",
                  "WA"),
        city = c("Bridgeport", "Tacoma")
      ),
      row.names = 1:2,
      class = c("dimension_table", "tbl_df",
                "tbl", "data.frame"),
      name = "where",
      type = "conformed"
    )
  )
})
