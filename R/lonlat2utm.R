#' Latlon to UTM zone
#'
#' From https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
#'
#' @param lonlat a vector with longitude and latitude
#' @export
lonlat2utm <- function(lonlat) {
  utm <- (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if (lonlat[2] > 0) {
    utm + 32600
  } else {
    utm + 32700
  }
}
