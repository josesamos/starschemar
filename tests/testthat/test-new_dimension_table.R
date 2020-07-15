context("test new_dimension_table")

test_that("new_dimension_table works", {
  dim <-
    new_dimension_table(mrs_age_test[, sd_mrs_age$dimension[[3]]], names(sd_mrs_age$dimension)[3])

  res <-
    structure(
      list(
        where_key = 1:3,
        REGION = c(1L, 1L, 9L),
        State = c("CT",
                  "CT", "WA"),
        City = c("Bridgepor", "Bridgeport", "Tacoma")
      ),
      row.names = c(NA,
                    3L),
      class = c("tbl_df", "tbl", "data.frame", "dimension_table"),
      name = "where",
      type = "general"
    )

  expect_equal(dim, res)
})
