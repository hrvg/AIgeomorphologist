#' Extracts mid-points from streamlines
#' @param spatial_data a `data.frame` with columns `coordx` and `coordy`
#' @param coordx `character`, column name for x coordinates
#' @param coordy `character`, column name for y coordinates
#' @param .crs a reference coordinate system to project the points to
#' @return a `SpatialPointsDataFrame`
#' @export
#' @keywords geospatial-data
to_spatial <- function(
  spatial_data,
  coordx = "POINT_X",
  coordy = "POINT_Y",
  .crs = sp::CRS("+proj=longlat +datum=WGS84")
  ) {
  if (class(spatial_data) != "data.frame") stop("`spatial_data` should be a `data.frame`")
  if (!coordx %in% colnames(spatial_data)) stop(paste(coordx, "not found in `spatial_data`"))
  if (!coordy %in% colnames(spatial_data)) stop(paste(coordy, "not found in `spatial_data`"))
  lonlat <- cbind(spatial_data[[coordx]], spatial_data[[coordy]])
  pts <- sp::SpatialPoints(lonlat, proj4string = .crs)
  pts$tmp <- spatial_data[[1]]
  pts@data <- spatial_data %>% dplyr::select(-c(coordx, coordy))
  pts$tmp <- NULL
  return(pts)
}
