context("test geomultistar")

test_that("geomultistar works", {
  gms <- geomultistar(ms = ms_mrs_test, geodimension = "where")

  expect_equal(class(gms), c("multistar", "geomultistar"))
  expect_equal(gms$geodimension, list(where = list(
    all_where = NULL,
    region = NULL,
    state = NULL,
    city = NULL
  )))
})
