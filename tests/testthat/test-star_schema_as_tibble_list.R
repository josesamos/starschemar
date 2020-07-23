context("test star_schema_as_tibble_list")

test_that("star_schema_as_tibble_list works", {
  tl <- star_schema_as_tibble_list(st_mrs_age_test)
  expect_equal(lapply(tl, length),
               list(
                 mrs_age = 6L,
                 when = 4L,
                 when_available = 4L,
                 where = 4L,
                 who = 2L
               ))
  expect_equal(
    lapply(tl, f <- function(x)
      length(x[[1]])),
    list(
      mrs_age = 24L,
      when = 9L,
      when_available = 9L,
      where = 3L,
      who = 5L
    )
  )

  tl <- star_schema_as_tibble_list(st_mrs_age_test, TRUE)
  expect_equal(
    lapply(tl, length),
    list(
      mrs_age = 6L,
      when = 4L,
      when_available = 4L,
      where = 4L,
      who = 2L,
      when_common = 4L
    )
  )
  expect_equal(
    lapply(tl, f <- function(x)
      length(x[[1]])),
    list(
      mrs_age = 24L,
      when = 9L,
      when_available = 9L,
      where = 3L,
      who = 5L,
      when_common = 9L
    )
  )
})
