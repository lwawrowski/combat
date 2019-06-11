#' Calculate beta coefficients for a straight line connecting two points. This function is used to calculate strength of attack with distance redcution.
#'
#' @param a first point - numeric vector of length of 2
#' @param b second point - numeric vector of length of 2
#' @return Beta coefficients for line which connects points a and b
#' @examples
#' linear_model(c(0, 10), c(10, 1))

linear_model <- function(a, b){

  assert_that(length(a) == length(b))
  assert_that(length(a) == 2)
  assert_that(length(b) == 2)
  assert_that(noNA(a))
  assert_that(noNA(b))
  assert_that(is.numeric(a))
  assert_that(is.numeric(b))

  x1 <- a[1]
  y1 <- a[2]

  x2 <- b[1]
  y2 <- b[2]

  assert_that((x2-x1) != 0, msg = "Cannot calculate coefficients because it is vertical line")

  b0 <- (y1*x2-y1*x1-y2*x1+y1*x1)/(x2-x1)
  b1 <- (y2-y1)/(x2-x1)

  betas <- round(c(b0, b1),4)

  return(betas)
}
