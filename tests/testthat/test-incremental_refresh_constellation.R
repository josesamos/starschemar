context("test incremental_refresh_constellation")

test_that("incremental_refresh_constellation works", {
  ct <- incremental_refresh_constellation(ct_mrs_test, st_mrs_age_w_test)

  expect_equal(
    lapply(ct$star$mrs_cause$dimension$where, length),
    list(
      where_key = 4L,
      region = 4L,
      state = 4L,
      city = 4L
    )
  )
  expect_equal(
    lapply(ct$star$mrs_cause$dimension$when_common, length),
    list(
      when_common_key = 16L,
      date = 16L,
      week = 16L,
      year = 16L
    )
  )
})
