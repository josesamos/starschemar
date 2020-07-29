context("test define_fact")

test_that("define_fact works", {
  sd <- dimensional_model()
  sd <- define_fact(sd,
                    name = "mrs_age",
                    measures = c("Deaths"))
  expect_equal(sd$fact, dm_mrs_age$fact)
})
