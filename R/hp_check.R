#' Calculate possible health points
#'
#' Calculate possible health points for information purposes
#'
#' @param player_id character ID of player
#' @param opponent_id character ID of opponent
#' @param units_data dataset contains information of all units in the game
#' @param dist_matrix_data distance matrix
#' @export
#' @importFrom assertthat assert_that
#' @return Possible number of health points in case of attack but without any boosters and discounts
#' @examples
#'
#' set.seed(123)
#' p1 <- draw_units(price_limit = 50, units_data = units)
#' p2 <- draw_units(price_limit = 50, units_data = units)
#' units_game <- draw_hexs(p1, p2)
#'
#' hp_check(player_id = "ID9", opponent_id = "ID14",
#' units_data = units_game, dist_matrix_data = dist_matrix)

hp_check <- function(player_id, opponent_id, units_data, dist_matrix_data){

  assert_that(is.data.frame(units_data))
  assert_that(is.matrix(dist_matrix_data))

  assert_that(is.character(player_id))
  assert_that(is.character(opponent_id))
  assert_that(nrow(units_data) > 0, msg = "There is no units in the dataset")

  assert_that(!is.null(units_data$id), msg = "There is no id variable in units_data. Check whether proper dataset was delivered.")
  assert_that(!is.null(units_data$attack), msg = "There is no attack variable in units_data. Check whether proper dataset was delivered.")
  assert_that(!is.null(units_data$defence), msg = "There is no defence variable in units_data. Check whether proper dataset was delivered.")
  assert_that(!is.null(units_data$hp_c), msg = "There is no hp_c variable in units_data. Check whether proper dataset was delivered.")
  assert_that(!is.null(units_data$range_min), msg = "There is no range_min variable in units_data. Check whether proper dataset was delivered.")
  assert_that(!is.null(units_data$range_max), msg = "There is no range_max variable in units_data. Check whether proper dataset was delivered.")

  p_id <- units_data[units_data$id == player_id,]
  o_id <- units_data[units_data$id == opponent_id,]

  assert_that(nrow(p_id) > 0, msg = paste0("There is no unit with ", player_id))
  assert_that(nrow(o_id) > 0, msg = paste0("There is no unit with ", opponent_id))

  hp_base <- 35*1.1^(p_id$attack-o_id$defence)
  hp_diff <- 1.005^(p_id$hp_c-o_id$hp_c)

  dist <- dist_matrix_data[player_id, opponent_id]

  # if opponent is out of range then attack is impossible, so result is 0
  range <- ifelse(dist %in% p_id$range_min:p_id$range_max, 1, 0)

  hp <- hp_base * hp_diff * range

  return(hp)
}
