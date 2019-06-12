#' Draw units
#'
#' Draw units according to indicated price
#'
#' @param price_limit limit of price within units can be sampled
#' @param units_data dataset contains information of available units
#' @return Dataset of drawned units according to price limit
#' @export
#' @importFrom dplyr sample_n
#' @importFrom assertthat assert_that
#' @examples
#'
#' player1 <- draw_units(price_limit = 50, units_data = units)

draw_units <- function(price_limit = 50, units_data){

  assert_that(is.numeric(price_limit))
  assert_that(price_limit > 0)
  # assert_that(is.data.frame(units_data))

  units_drawned <- data.frame()

  points_left <- price_limit

  min_unit_price <- min(units_data$price)

  assert_that(price_limit >= min_unit_price, msg = paste0("Price limit is too low - minimum price of unit is equal to ", min_unit_price))

  while(points_left >= min_unit_price && nrow(units_drawned) < 10){

    unit_draw <- sample_n(units_data[units_data$price <= points_left, ], 1)

    units_drawned <- rbind(units_drawned, unit_draw)

    points_left <- points_left - as.numeric(unit_draw$price)

    # only one boss
    boss <- nrow(units_drawned[units_drawned$name == "boss",])

    if(boss != 0 && nrow(units_data[units_data$name == "boss",]) != 0){
      units_data <- units_data[units_data$name != "boss",]
    }
  }

  units_drawned$name <- paste0(units_drawned$name, 1:nrow(units_drawned))

  return(units_drawned)
}
