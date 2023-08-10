context("test update_record")

test_that("update_record works", {
  where <- st_mrs_age_test |>
    get_dimension("where")

  updates <- record_update_set() |>
    update_record(dimension = where,
                  old = 1,
                  values = c("1", "CT", "Bridgeport")) |>
    update_record(dimension = where,
                  old = 1,
                  values = c("1", "CT", "Bridgeport"))

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
