context("test rename_dimension_attributes")

test_that("rename_dimension_attributes works", {
  st <-
    st_mrs_age_test %>% rename_dimension_attributes(
      name = "when",
      attributes = c("week", "year"),
      new_names = c("w", "y")
    )

  expect_equal(
    attributes(st$dimension$when),
    list(
      names = c("when_key", "week_ending_date", "w", "y"),
      row.names = integer(0),
      class = c("tbl_df", "tbl", "data.frame", "dimension_table"),
      name = "when",
      type = "role",
      role_playing = "when_common"
    )
  )
})
