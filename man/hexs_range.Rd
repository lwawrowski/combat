% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hexs_range.R
\name{hexs_range}
\alias{hexs_range}
\title{Hexs in range for unit}
\usage{
hexs_range(hex_id, range_min, range_max, dist_matrix_data)
}
\arguments{
\item{hex_id}{id of hex where there is a unit}

\item{range_min}{minimum range of the unit}

\item{range_max}{maximum range of the unit}

\item{dist_matrix_data}{distance matrix}
}
\value{
Vector with IDs of hexs which are in range of unit
}
\description{
Return a vector of hexs which are in range of unit
}
\examples{
hexs_range(hex_id = "ID8", range_min = 1, range_max = 5, dist_matrix_data = dist_matrix)
}
