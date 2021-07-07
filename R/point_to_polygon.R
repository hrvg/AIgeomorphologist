#' Transform a `sf` point into a `geojson` polygon
#' @param point a `sf` object
#' @param radius `numeric` the radius of the spatial buffer from `raster::buffer()` in meters
#' @param default_crs `numeric`, default CRS
#' @param ... pass to `sf::
#' @return a `sf` `Polygon`
#' @keywords geospatial-data
#' @importFrom methods as
#' @export
point_to_polygon <- function(point, radius = 5, default_crs = 4326, ...) {
  if (!"sf" %in% class(point)) stop("invalid class: point is not of class 'sf'")
  if (sf::st_crs(point) %>% is.na()) point <- point %>% sf::st_set_crs(default_crs)
  crs_utm <- point %>%
    sf::st_coordinates() %>%
    lonlat2utm() %>%
    sf::st_crs()
  point %>%
    sf::st_transform(crs_utm) %>%
    sf::st_buffer(radius, ...) %>%
    sf::st_geometry() %>%
    sf::st_as_sf() %>%
    sf::st_transform(default_crs)
}

#' Latlon to UTM zone
#' From https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
#' @param lonlat a two-element `vector` with latitude and longitude
#' @keywords geospatial-data
#' @export
lonlat2utm <- function(lonlat) {
  utm <- (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else {
    utm + 32700
  }
}
