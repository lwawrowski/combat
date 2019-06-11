#' Calculate distance reduction value due to range and accuracy of the unit and distance from the target.
#'
#' @param range_min minimum range of the unit
#' @param range_max maximum range of the unit
#' @param accuracy accuracy of the unit
#' @param distance distance between unit and target
#' @return distance reduction value from [0;1] interval
#' @examples
#' distance_reduction(1, 5, 6, 4)

distance_reduction <- function(range_min, range_max, accuracy, distance){

  assert_that(is.number(range_min))
  assert_that(is.number(range_max))
  assert_that(is.number(accuracy))
  assert_that(is.number(distance))

  assert_that(range_min > 0)
  assert_that(range_max > 0)
  assert_that(accuracy %in% 1:10, msg = "Accuracy must be integer from 1 to 10.")
  assert_that(distance > 0)

  assert_that(noNA(range_min))
  assert_that(noNA(range_max))
  assert_that(noNA(accuracy))
  assert_that(noNA(distance))

  a <- c(range_min,1)
  b <- c(range_max,accuracy/10)

  betas <- linear_model(a,b)

  y <- betas[2]*distance+betas[1]

  return(y)
}
