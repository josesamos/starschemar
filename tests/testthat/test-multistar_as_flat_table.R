context("test multistar_as_flat_table")

test_that("multistar_as_flat_table works", {
  ft <- ms_mrs_test |>
    multistar_as_flat_table(fact = "mrs_age")

  expect_equal(
    names(ft),
    c(
      "age_range",
      "region",
      "state",
      "city",
      "data_availability_date",
      "data_availability_week",
      "data_availability_year",
      "week_ending_date",
      "week",
      "year",
      "deaths",
      "nrow_agg"
    )
  )
  expect_equal(length(ft[[1]]), 24)

  ms <- dimensional_query(ms_mrs_test) |>
    select_dimension(name = "where",
                     attributes = c("city", "state")) |>
    select_dimension(name = "when",
                     attributes = c("year", "week")) |>
    select_fact(
      name = "mrs_age",
      measures = c("deaths")
    ) |>
    select_fact(
      name = "mrs_cause",
      measures = c("pneumonia_and_influenza_deaths", "other_deaths")
    ) |>
    run_query()
  ft <- ms |>
    multistar_as_flat_table()

  expect_equal(
    names(ft),
    c(
      "year",
      "week",
      "city",
      "state",
      "deaths",
      "nrow_agg",
      "mrs_cause_pneumonia_and_influenza_deaths",
      "mrs_cause_other_deaths",
      "mrs_cause_nrow_agg"
    )
  )
  expect_equal(length(ft[[1]]), 6)
})
