#' Calculate possible health points
#'
#' Calculate possible health points for information purposes
#'
#' @param player_id character ID of player
#' @param opponent_id character ID of opponent
#' @param units_data dataset contains information of all units in the game
#' @param dist_matrix_data distance matrix
#' @return Possible number of health points in case of attack but without any boosters and discounts
#' @examples
#'
#' check_hp("")

check_hp <- function(player_id, opponent_id, units_data, dist_matrix_data){

  assert_that(is.character(player_id))
  assert_that(is.character(opponent_id))
  assert_that(nrow(units_data) > 0, msg = "There is no units in the dataset")

  p_id <- units_data[units_data$id == player_id,]
  o_id <- units_data[units_data$id == opponent_id,]

  assert_that(nrow(p_id) > 0, msg = paste0("There is no unit with ", player_id, " ID"))
  assert_that(nrow(o_id) > 0, msg = paste0("There is no unit with ", opponent_id, " ID"))

  # check names of variables

  hp_base <- 35*1.1^(p_id$attack-o_id$defence)
  hp_diff <- 1.005^(p_id$hp_c-o_id$hp_c)

  dist <- dist_matrix_data[player_id, opponent_id]

  # if opponent is out of range then attack is impossible, so result is 0
  range <- ifelse(dist %in% p_id$range_min:p_id$range_max, 1, 0)

  hp <- hp_base * hp_diff * range

  return(hp)
}
