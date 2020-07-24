context("test incremental_refresh_star_schema")

test_that("incremental_refresh_star_schema works", {
  st <- st_mrs_age_test
  st$fact$mrs_age$deaths <- 1
  st$fact$mrs_age$nrow_agg <- 1

  stw <- st_mrs_age_w_test
  stw$fact$mrs_age$deaths <- 2
  stw$fact$mrs_age$nrow_agg <- 2

  st$fact$mrs_age$nrow_agg
  stw$fact$mrs_age$nrow_agg


  st1 <- incremental_refresh_star_schema(st, stw, existing = "ignore")
  st2 <- incremental_refresh_star_schema(st, stw, existing = "replace")
  st3 <- incremental_refresh_star_schema(st, stw, existing = "group")
  st4 <- incremental_refresh_star_schema(st, stw, existing = "delete")

  r1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  r2 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 1,
          1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  r3 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 3, 3, 1, 1, 1,
          1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  r4 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

  expect_equal(st1$fact$mrs_age$nrow_agg, r1)
  expect_equal(st1$fact$mrs_age$deaths, r1)
  expect_equal(st2$fact$mrs_age$nrow_agg, r2)
  expect_equal(st2$fact$mrs_age$deaths, r2)
  expect_equal(st3$fact$mrs_age$nrow_agg, r3)
  expect_equal(st3$fact$mrs_age$deaths, r3)
  expect_equal(st4$fact$mrs_age$nrow_agg, r4)
  expect_equal(st4$fact$mrs_age$deaths, r4)
})
