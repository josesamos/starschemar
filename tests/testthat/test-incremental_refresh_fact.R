context("test incremental_refresh_fact")

test_that("incremental_refresh_fact works", {
  facts1 <-
    st_mrs_age_test$fact$mrs_age[st_mrs_age_test$fact$mrs_age$when_available_key < 6,]
  facts2 <-
    st_mrs_age_test$fact$mrs_age[st_mrs_age_test$fact$mrs_age$when_available_key > 2 &
                                   st_mrs_age_test$fact$mrs_age$when_available_key <= 6,]

  facts1$deaths <- 1
  facts1$nrow_agg <- 1

  facts2$deaths <- 2
  facts2$nrow_agg <- 2

  ft1 <- incremental_refresh_fact(facts1, facts2, existing = "ignore")

  ft2 <- incremental_refresh_fact(facts1, facts2, existing = "replace")

  ft3 <- incremental_refresh_fact(facts1, facts2, existing = "group")

  ft4 <- incremental_refresh_fact(facts1, facts2, existing = "delete")

  r1 <- c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
  r2 <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
  r3 <- c(1, 1, 1, 3, 3, 3, 2, 2, 2, 2, 2)
  r4 <- c(1, 1, 1, 2, 2, 2, 2, 2)

  expect_equal(ft1$deaths, r1)
  expect_equal(ft1$nrow_agg, r1)

  expect_equal(ft2$deaths, r2)
  expect_equal(ft2$nrow_agg, r2)

  expect_equal(ft3$deaths, r3)
  expect_equal(ft3$nrow_agg, r3)

  expect_equal(ft4$deaths, r4)
  expect_equal(ft4$nrow_agg, r4)
})
