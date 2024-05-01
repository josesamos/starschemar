context("test replace_dimension_in_facts")

test_that("replace_dimension_in_facts works", {
  st <- replace_dimension_in_facts(st_mrs_age_test, "when", ct_mrs_test$dimension$when, set_type_conformed = TRUE)

  expect_equal(
    attributes(st$dimension$when),
    list(
      names = c("when_key", "week_ending_date", "week", "year"),
      row.names = integer(0),
      name = "when",
      type = c("role", "conformed"),
      role_playing = "when_common",
      class = c("dimension_table", "tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(nrow(st$dimension$when_common), 13)
})
