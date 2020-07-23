context("test prepare_join")

test_that("prepare_join works", {
  tb <- prepare_join(mrs_age_test)

  expect_equal(unique(unlist(
    dplyr::summarise_all(tb, class), use.names = FALSE
  )), "character")
  expect_equal(tb$`Data Availability Date`[4], "___UNKNOWN___")
})
