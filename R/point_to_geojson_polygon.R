#' Transform a `sf` point into a `geojson` polygon
#' @param point a `sf` object
#' @param radius `numeric` the radius of the spatial buffer from `raster::buffer()` in meters
#' @return a `geojson` `Polygon`
#' @importFrom methods as
#' @export
point_to_geojson_polygon <- function(point, radius = 500){
	if(!"sf" %in% class(point)) stop("invalid class: point is not of class 'sf'")
	raster::buffer(point %>% as("Spatial"), radius) %>%
	raster::extent() %>% 
	as("SpatialPolygons") %>% 
	sf::st_as_sf() %>% 
	geojsonsf::sf_geojson()
}