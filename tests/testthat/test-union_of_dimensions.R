context("test union_of_dimensions")

test_that("union_of_dimensions works", {
  d <-
    union_of_dimensions(list(
      st_mrs_age_test$dimension$who,
      st_mrs_age_test$dimension$who
    ),
    "test")

  expect_equal(d,
               structure(
                 list(
                   test_key = 1:5,
                   who_key = 1:5,
                   age_range = c("<1 year",
                                 "1-24 years", "25-44 years", "45-64 years", "65+ years")
                 ),
                 row.names = c(NA,
                               5L),
                 class = c("dimension_table", "tbl_df", "tbl", "data.frame"),
                 name = "test",
                 type = "role_playing"
               ))
})
