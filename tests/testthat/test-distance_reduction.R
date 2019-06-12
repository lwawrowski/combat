context("Distance reduction")

test_that("check results", {
  expect_equal(distance_reduction(range_min = 1, range_max = 5, accuracy = 6, distance = 5), 0.6)
  expect_lte(distance_reduction(range_min = 1, range_max = 2, accuracy = 9, distance = 5), 1)
  expect_gte(distance_reduction(range_min = 1, range_max = 10, accuracy = 6, distance = 5), 0)
})

test_that("check errors", {
  expect_error(distance_reduction(range_min = NA, range_max = 5, accuracy = 6, distance = 5))
  expect_error(distance_reduction(range_min = 1, range_max = 5, accuracy = 11, distance = 5))
  expect_error(distance_reduction(range_min = -1, range_max = 5, accuracy = 8, distance = 5))
  expect_error(distance_reduction(range_min = 5, range_max = 1, accuracy = 6, distance = 5))
  expect_error(distance_reduction(range_min = 1, range_max = 10, accuracy = 10, distance = 100))
})

