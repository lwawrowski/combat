#' Distance matrix
#'
#' Distance matrix for hex map with 357 hexs (16 x 23)
#' @format A matrix with 357 rows and 357 columns
"dist_matrix"

#' Units data
#'
#' Examples of units in game. The variables are as follows:
#'
#' \describe{
#' \item{name}{character; name of unit}
#' \item{hp}{numeric; initial health points}
#' \item{mobil}{numeric; mobility points}
#' \item{attack}{numeric; attack points}
#' \item{defence}{numeric; defence points}
#' \item{range_min}{numeric; minimum range of unit}
#' \item{range_max}{numeric; maximum range of unit}
#' \item{price}{numeric; price of unit}
#' \item{acc}{numeric; accuracy points}
#' \item{acc_min}{numeric; missing shot chance}
#' \item{acc_max}{numeric; super shot chance}
#' }
#'
#' @format A data frame with 5 rows and 11 variables
"units"
