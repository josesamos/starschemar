context("test role_dimension")

test_that("role_dimension works", {
  d <- role_dimension(st_mrs_age_test$dimension$where, "test")
  res <-
    structure(
      list(
        where_key = logical(0),
        region = logical(0),
        state = logical(0),
        city = logical(0)
      ),
      row.names = integer(0),
      class = c("dimension_table", "tbl_df",
                "tbl", "data.frame"),
      name = "where",
      type = "role",
      role_playing = "test"
    )
  expect_equal(d, res)
})
