context("test enrich_dimension_import_test")

test_that("enrich_dimension_import_test works", {
  tb <-
    enrich_dimension_export(st_mrs_age_test,
                            name = "when_common",
                            attributes = c("week", "year"))
  tb <- tibble::add_column(tb, x = 1, y = 2, z = 3)[-1,]
  tb2 <-
    enrich_dimension_import_test(st_mrs_age_test, name = "when_common", tb)

  expect_equal(
    tb2,
    structure(
      list(
        when_common_key = 1:2,
        date = structure(c(-2917,-2916), class = "Date"),
        week = c("01", "01"),
        year = c("1962",
                 "1962")
      ),
      row.names = c(NA,-2L),
      class = c("dimension_table", "tbl_df", "tbl",
                "data.frame"),
      name = "when_common",
      type = "role_playing"
    )
  )
})
