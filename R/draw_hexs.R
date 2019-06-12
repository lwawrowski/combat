#' Draw hexs
#'
#' Draw hexs from left and right side of the map for drawned units
#'
#' @param player1 drawned units for player 1
#' @param player2 drawned units for player 2
#' @return Dataset of drawned units with hexs
#' @export
#' @importFrom dplyr mutate
#' @examples
#'
#' player1 <- draw_units(price_limit = 50, units_data = units)
#' player2 <- draw_units(price_limit = 50, units_data = units)
#'
#' draw_hexs(player1, player2)

draw_hexs <- function(player1, player2){

  assert_that(is.data.frame(player1))
  assert_that(is.data.frame(player2))

  units_n_p1 <- nrow(player1)
  units_n_p2 <- nrow(player2)

  # add ID
  units_p1 <- data.frame(player1, player=-1, unit_id=paste0("p1id",1:units_n_p1))
  units_p2 <- data.frame(player2, player=1, unit_id=paste0("p2id",1:units_n_p2))
  units_battle <- rbind(units_p1, units_p2)

  # draw hex
  hexs_battle <- data.frame(unit_id=c(paste0("p1id",1:units_n_p1),paste0("p2id",1:units_n_p2)),
                            id=c(sample(paste0("ID", 1:16), units_n_p1),sample(paste0("ID", 342:357), units_n_p2)))

  units_battle_hexs <- merge(units_battle, hexs_battle, by="unit_id")

  units_battle_hexs <- units_battle_hexs %>%
    mutate(hp_c=hp,
           id=as.character(id))

  return(units_battle_hexs)

}
