context("test define_dimension")

test_that("define_dimension works", {
  sd <- dimensional_model()
  sd <- define_dimension(sd,
                         name = "when",
                         attributes = c("Week Ending Date",
                                        "WEEK",
                                        "Year"))
  sd <- define_dimension(
    sd,
    name = "when_available",
    attributes = c(
      "Data Availability Date",
      "Data Availability Week",
      "Data Availability Year"
    )
  )
  sd <- define_dimension(sd,
                         name = "where",
                         attributes = c("REGION",
                                        "State",
                                        "City"))
  sd <- define_dimension(sd, name = "who",
                         attributes = c("Age Range"))
  expect_equal(sd$dimension, dm_mrs_age$dimension)
})
