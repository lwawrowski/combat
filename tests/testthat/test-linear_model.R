context("Linear model")

test_that("check results", {
  expect_equal(linear_model(c(1,2), c(10,2)), c(2,0))
})

test_that("check errors", {
  expect_error(linear_model(c(1,2), c(10,NA)))
  expect_error(linear_model(c(2,10), c(2,20)))
})

