context("Hexs mobility")

set.seed(123)
load("units.RData")
p1 <- draw_units(price_limit = 50, units_data = units)
p2 <- draw_units(price_limit = 50, units_data = units)
units_game <- draw_hexs(p1, p2)

test_that("check results", {
  expect_length(hexs_mobility(hex_id = "ID9", units_data = units_game, dist_matrix_data = dist_matrix), 126)
  expect_length(hexs_mobility(hex_id = "ID344",mobility = 2, units_data = units_game, dist_matrix_data = dist_matrix), 10)
  expect_length(hexs_mobility(hex_id = "ID9",mobility = 0, units_data = units_game, dist_matrix_data = dist_matrix), 0)
  expect_equal(hexs_mobility(hex_id = "ID344", units_data = units_game, dist_matrix_data = dist_matrix)[1], c("ID282" = 3))
  expect_equal(hexs_mobility(hex_id = "ID344",mobility = 2, units_data = units_game, dist_matrix_data = dist_matrix)[1], c("ID312" = 2))
})

test_that("check errors", {
  expect_error(hexs_mobility(hex_id = "ID8", units_data = units_game, dist_matrix_data = dist_matrix))
  expect_error(hexs_mobility(hex_id = "ID9", units_data = units, dist_matrix_data = dist_matrix))
  expect_error(hexs_mobility(hex_id = "ID9", mobility = -10, units_data = units, dist_matrix_data = dist_matrix))
  expect_error(hexs_mobility(hex_id = "ID9", mobility = NA, units_data = units, dist_matrix_data = dist_matrix))
  expect_error(hexs_mobility(hex_id = 9, mobility = 5, units_data = units, dist_matrix_data = dist_matrix))
})

