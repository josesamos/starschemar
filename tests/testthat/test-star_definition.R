context("test star_definition")

test_that("star_definition works", {
  sd <- star_definition()
  expect_equal(class(sd), "star_definition")
})
