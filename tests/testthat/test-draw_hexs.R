context("Draw hexs")

load("units.RData")
p1 <- draw_units(price_limit = 50, units_data = units)
p2 <- draw_units(price_limit = 50, units_data = units)

test_that("check results", {
  expect_s3_class(draw_hexs(player1 = p1, player2 = p2), "data.frame")
  expect_true(nrow(draw_hexs(player1 = p1, player2 = p2)) > 0)

})

test_that("check errors", {
  expect_error(draw_hexs(player1 = p1, player2 = NA))
})

