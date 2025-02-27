% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{id_tiles}
\alias{id_tiles}
\title{Sentinel-2 tile ids for the statewide channel type dataset}
\format{
A vector of \code{character}
}
\usage{
data(id_tiles)
}
\description{
Sentinel-2 tile ids for the statewide channel type dataset
}
\details{
The following code generates the data:\preformatted{labelled_points <- SSCT_data \%>\% to_spatial() \%>\% sf::st_as_sf()
id_tiles <- s2_list(
  spatial_extent = labelled_points, 
  time_period = "full",
  level = "L2A",
  availability = "online"
  ) \%>\% 
  as.data.frame() \%>\% 
  dplyr::pull(id_tile) \%>\%
  unique()
}
}
\keyword{dataset}
