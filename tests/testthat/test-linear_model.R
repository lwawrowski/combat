context("Linear model")

test_that("check results", {
  expect_equal(linear_model(a = c(1,2), b = c(10,2)), c(2,0))
  expect_length(linear_model(a = c(1,5), b = c(10,1)), 2)
})

test_that("check errors", {
  expect_error(linear_model(a = c(1,2), b = c(10,NA)))
  expect_error(linear_model(a = c(2,10), b = c(2,20)))
  expect_error(linear_model(a = c(2,10,5), b = c(3,20)))
  expect_error(linear_model(a = c("2","10"), b = c(5,20)))
})

