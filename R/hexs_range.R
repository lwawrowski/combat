#' Hexs in range for unit
#'
#' Return a vector of hexs which are in range of unit
#'
#' @param hex_id id of hex where there is a unit
#' @param range_min minimum range of the unit
#' @param range_max maximum range of the unit
#' @param dist_matrix_data distance matrix
#' @export
#' @return Vector with IDs of hexs which are in range of unit
#' @examples
#' hexs_range(hex_id = "ID8", range_min = 1, range_max = 5, dist_matrix_data = dist_matrix)

hexs_range <- function(hex_id, range_min, range_max, dist_matrix_data){

  assert_that(is.character(hex_id))
  assert_that(is.number(range_min))
  assert_that(is.number(range_max))
  assert_that(is.matrix(dist_matrix_data))

  assert_that(range_max > range_min, msg = "Maximum range must be greater than minimum range")
  assert_that(range_min > 0)
  assert_that(range_max > 0)

  hexs <- names(which(dist_matrix_data[hex_id,] >= range_min & dist_matrix_data[hex_id,] <= range_max))

  return(hexs)
}
