#' Statewide channel type dataset
#' @docType data
#' @usage data(SSCT_data)
#' @format A `data.frame`
#' @keywords dataset
"SSCT_data"

#' Sentinel-2 tile ids for the statewide channel type dataset
#' 
#' @details
#' The following code generates the data:
#' 
#' ```
#' labelled_points <- SSCT_data %>% to_spatial() %>% sf::st_as_sf()
#' id_tiles <- s2_list(
#'   spatial_extent = labelled_points, 
#'   time_period = "full",
#'   level = "L2A",
#'   availability = "online"
#'   ) %>% 
#'   as.data.frame() %>% 
#'   dplyr::pull(id_tile) %>%
#'   unique()
#' ```
#' 
#' @docType data
#' @usage data(id_tiles)
#' @format A vector of `character`
#' @keywords dataset
"id_tiles"

#' Search results between "2019-06-01" and "2020-10-01" for the id_tiles
#' 
#' @details
#' The following code generates the data:
#' 
#' ```
#' search_results <- s2_list(
#'   tile = id_tiles, 
#'   time_interval = c(as.Date("2019-06-01"), as.Date("2020-10-01")),
#'   time_period = "full",
#'   level = "L2A",
#'   availability = "online"
#' ) 
#' ```
#' @docType data
#' @usage data(search_results)
#' @format A `safelist`
#' @keywords dataset
"search_results"