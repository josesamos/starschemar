context("test define_fact")

test_that("define_fact works", {
  sd <- star_definition()
  sd <- define_fact(sd,
                    name = "mrs_age",
                    measures = c("Deaths"))
  expect_equal(sd$fact, sd_mrs_age$fact)
})
