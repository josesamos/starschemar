context("test snake_case_fact")

test_that("snake_case_fact works", {
  st <- star_schema(mrs_cause_test, dm_mrs_cause)
  d <- snake_case_fact(st$fact$mrs_cause)

  expect_equal(
    sort(colnames(d)),
    c(
      "nrow_agg",
      "other_deaths",
      "pneumonia_and_influenza_deaths",
      "when_available_key",
      "when_key",
      "when_received_key",
      "where_key"
    )
  )
})
