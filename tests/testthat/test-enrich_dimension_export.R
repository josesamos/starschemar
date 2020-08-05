context("test enrich_dimension_export")

test_that("enrich_dimension_export works", {
  tb <-
    enrich_dimension_export(st_mrs_age_test,
                            name = "when_common",
                            attributes = c("week", "year"))

  expect_equal(
    tb,
    structure(
      list(
        week = c("01", "02", "Unknown", "03", "04", "05"),
        year = c("1962", "1962", "Unknown", "1962", "1962", "1962")
      ),
      row.names = c(NA,-6L),
      name = "when_common",
      type = "role_playing",
      class = c("tbl_df",
                "tbl", "data.frame")
    )
  )
})
