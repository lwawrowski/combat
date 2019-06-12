#' Mobility hexs for unit
#'
#' Return a vector of hexs which are empty, so the unit can move on them.
#'
#' @param hex_id id of hex where there is a unit
#' @param mobility mobility of unit, default is NULL and then mobility is taken from units_data
#' @param units_data dataset which contains information about units with its hexs
#' @param dist_matrix_data distance matrix
#' @export
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.number
#' @return Vector with IDs of hexs where unit can move.
#' @examples
#' hexs_mobility(hex_id = "ID8", units_data = units_game, dist_matrix_data = dist_matrix)
#' hexs_mobility(hex_id = "ID8", mobility = 2, units_data = units_game, dist_matrix_data = dist_matrix)

hexs_mobility <- function(hex_id, mobility = NULL, units_data, dist_matrix_data){

  hex_id <- as.character(hex_id)

  assert_that(is.character(hex_id))
  assert_that(is.data.frame(units_data))
  assert_that(is.matrix(dist_matrix_data))

  assert_that(hex_id %in% units_data$id, msg = paste0("There is no unit with ", hex_id, " in units_data"))

  if(!is.null(mobility)){
    assert_that(is.number(mobility), msg = "Mobility must be a number")
    assert_that(mobility >= 0, msg = "Mobility must be greater or equal zero")
  }

  if(is.null(mobility)){
    mobility <- as.numeric(units_data$mobil[units_data$id == hex_id])
  }

  hexs_all <- dist_matrix_data[hex_id,dist_matrix_data[hex_id,] <= mobility]
  hexs_no_units <- hexs_all[!(names(hexs_all) %in% units_data$id)]
  return(hexs_no_units)
}
