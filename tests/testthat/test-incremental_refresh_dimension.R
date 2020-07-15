context("test incremental_refresh_dimension")

test_that("incremental_refresh_dimension works", {
  d <- st_mrs_age_test$dimension$where
  d$city[3] <- "test"
  d <-
    incremental_refresh_dimension(st_mrs_age_test$dimension$where, d)

  expect_equal(sort(d$city), c("Bridgepor", "Bridgeport", "Tacoma", "test"))
  expect_equal(d$where_key, 1:4)
})
