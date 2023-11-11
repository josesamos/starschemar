context("test common")

test_that("common works", {
  expect_equal(validate_names(
    defined_names = c("a", "b", "c"),
    names = "a",
    concept = 'name',
    repeated = FALSE
  ),
  "a")
  expect_equal(
    validate_names(
      defined_names = c("a", "b", "c"),
      names = NULL,
      concept = 'name',
      repeated = FALSE
    ),
    c("a", "b", "c")
  )
  expect_equal(
    validate_names(
      defined_names = c("a", "b", "c"),
      names = c("a", "a"),
      concept = 'name',
      repeated = TRUE
    ),
    c("a", "a")
  )
  expect_equal({
    res <- tryCatch(
      validate_names(
        defined_names = c("a", "b", "c"),
        names = "d",
        concept = 'name',
        repeated = FALSE
      ),
      error = function(e)
        1
    )
    res
  },
  1)
})
