context("Hexs range")

last_element <- function(x){

  x[length(x)]

}

test_that("check results", {
  expect_length(hexs_range(hex_id = "ID1", range_min = 1, range_max = 3, dist_matrix_data = dist_matrix), 13)
  expect_equal(last_element(hexs_range(hex_id = "ID1", range_min = 1, range_max = 3, dist_matrix_data = dist_matrix)), "ID63")
  expect_equal(hexs_range(hex_id = "ID357", range_min = 1, range_max = 3, dist_matrix_data = dist_matrix)[1], "ID295")
  expect_equal(hexs_range(hex_id = "ID357", range_min = 1, range_max = 1, dist_matrix_data = dist_matrix), c("ID341", "ID356"))
})

test_that("check errors", {
  expect_error(hexs_range(hex_id = "ID357", range_min = 3, range_max = 1, dist_matrix_data = dist_matrix))
  expect_error(hexs_range(hex_id = "ID457", range_min = 1, range_max = 3, dist_matrix_data = dist_matrix))
  expect_error(hexs_range(hex_id = "ID357", range_min = 1, range_max = 2, dist_matrix_data = units))
  expect_error(hexs_range(hex_id = "ID35", range_min = 1, range_max = NA, dist_matrix_data = dist_matrix))
  expect_error(hexs_range(hex_id = 35, range_min = 1, range_max = 5, dist_matrix_data = dist_matrix))
})

