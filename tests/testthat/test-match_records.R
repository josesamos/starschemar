context("test match_records")

test_that("match_records works", {
  library(tidyr)

  where <- st_mrs_age_test %>%
    get_dimension("where")

  updates <- record_update_set() %>%
    match_records(dimension = where,
                  old = 1,
                  new = 2) %>%
    match_records(dimension = where,
                  old = 1,
                  new = 2)

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
