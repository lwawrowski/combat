context("Distance reduction")

test_that("check results", {
  expect_equal(distance_reduction(1,5,6,5), 0.6)
})

test_that("check errors", {
  expect_error(distance_reduction(NA, 5, 6, 5))
  expect_error(distance_reduction(1, 5, 11, 5))
  expect_error(distance_reduction(-1, 5, 11, 5))
})

