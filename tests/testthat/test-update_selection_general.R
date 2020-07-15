context("test update_selection_general")

test_that("update_selection_general works", {
  library(tidyr)

  where <- st_mrs_age_test %>%
    get_dimension("where")

  updates <- record_update_set() %>%
    update_selection_general(
      dimension = where,
      columns_old = c("region", "state", "city"),
      old_values = c("1", "CT", "Bridgepor"),
      columns_new = c("region", "state", "city"),
      new_values = c("1", "CT", "Bridgeport")
    ) %>%
    update_selection_general(
      dimension = where,
      columns_old = c("region", "state", "city"),
      old_values = c("1", "CT", "Bridgepor"),
      columns_new = c("region", "state", "city"),
      new_values = c("1", "CT", "Bridgeport")
    )

  res <-
    structure(list(
      dimension = "where",
      old = c(
        region = "1",
        state = "CT",
        city = "Bridgepor"
      ),
      new = c(
        region = "1",
        state = "CT",
        city = "Bridgeport"
      )
    ), class = "record_update")

  expect_equal(updates[[1]], res)
  expect_equal(updates[[2]], res)
})
