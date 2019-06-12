context("Draw units")

load("units.RData")

test_that("check results", {
  expect_s3_class(draw_units(price_limit = 50, units_data = units), "data.frame")
  expect_true(nrow(draw_units(price_limit = 50, units_data = units)) > 0)

})

test_that("check errors", {
  expect_error(draw_units(price_limit = -50, units_data = units))
})

