context("HP check")

set.seed(123)
load("units.RData")
p1 <- draw_units(price_limit = 50, units_data = units)
p2 <- draw_units(price_limit = 50, units_data = units)
units_game <- draw_hexs(p1, p2)

test_that("check results", {
  expect_equal(hp_check(player_id = "ID9", opponent_id = "ID6", units_data = units_game, dist_matrix_data = dist_matrix), 46.585)
  expect_equal(hp_check(player_id = "ID9", opponent_id = "ID344", units_data = units_game, dist_matrix_data = dist_matrix), 0)
})

test_that("check errors", {
  expect_error(hp_check(player_id = "ID9", opponent_id = "ID1", units_data = units_game, dist_matrix_data = dist_matrix))
  expect_error(hp_check(player_id = "ID9", opponent_id = "ID1", units_data = units, dist_matrix_data = dist_matrix))
})
