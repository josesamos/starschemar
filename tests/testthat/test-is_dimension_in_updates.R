context("test is_dimension_in_updates")

test_that("is_dimension_in_updates works", {
  where <- st_mrs_age_test |>
    get_dimension("where")

  updates <- record_update_set() |>
    match_records(dimension = where,
                  old = 1,
                  new = 2)

  expect_equal(is_dimension_in_updates(updates, "where"), TRUE)
  expect_equal(is_dimension_in_updates(updates, "who"), FALSE)
})
