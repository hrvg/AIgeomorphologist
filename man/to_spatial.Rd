% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_spatial.R
\name{to_spatial}
\alias{to_spatial}
\title{Extracts mid-points from streamlines}
\usage{
to_spatial(
  spatial_data,
  coordx = "POINT_X",
  coordy = "POINT_Y",
  .crs = sp::CRS("+proj=longlat +datum=WGS84")
)
}
\arguments{
\item{spatial_data}{a \code{data.frame} with columns \code{coordx} and \code{coordy}}

\item{coordx}{\code{character}, column name for x coordinates}

\item{coordy}{\code{character}, column name for y coordinates}

\item{.crs}{a reference coordinate system to project the points to}
}
\value{
a \code{SpatialPointsDataFrame}
}
\description{
Extracts mid-points from streamlines
}
\keyword{geospatial-data}
