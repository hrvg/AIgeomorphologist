devtools::load_all()
library(foreach)
library(doFuture)

safe_dir <- "/media/hguillon/UCD eFlow BigData1/S2_data/SAFE/" # folder to store downloaded SAFE
out_dir  <- "/media/hguillon/UCD eFlow BigData1/S2_data/sen2r_output/" # output folder

safe_dirs <- list.dirs(safe_dir, full.names = TRUE, recursive = FALSE)

current_tile <- sample(id_tiles, 1)
print(current_tile)

current_dirs <- safe_dirs[grepl(current_tile, safe_dirs)]

dops <- sen2r::safe_getMetadata(current_dirs, info = "fileinfo") %>%
  dplyr::pull(.data$sensing_datetime)

fpath <- current_dirs %>% sample(1)

# "B4" "B3" "B2" "B8"
s2_10 <- stars::read_stars(file.path(fpath, "MTD_MSIL2A.xml"), proxy = TRUE, sub = 1)

# "B5"  "B6"  "B7"  "B8A" "B11" "B12" "AOT" "CLD" "SCL" "SNW" "WVP"
s2_20 <- stars::read_stars(file.path(fpath, "MTD_MSIL2A.xml"), proxy = TRUE, sub = 2) 

# "B1"  "B9"  "AOT" "CLD" "SCL" "SNW" "WVP"
s2_60 <- stars::read_stars(file.path(fpath, "MTD_MSIL2A.xml"), proxy = TRUE, sub = 3)


s2_tci <- stars::read_stars(file.path(fpath, "MTD_MSIL2A.xml"), proxy = TRUE, sub = 4)

labelled_points <- SSCT_data %>% to_spatial() %>% sf::st_as_sf()
ind <- sapply(sf::st_intersects(labelled_points, sen2r::s2_tiles()), function(z) if (length(z)==0) NA_integer_ else z[1])
id_tiles <- sen2r::s2_tiles()[ind, ] %>% dplyr::pull(tile_id)
labelled_points <- labelled_points %>% dplyr::mutate(id_tile = id_tiles)

random_point <- labelled_points %>%
  dplyr::filter(id_tile == current_tile) %>%
  dplyr::sample_n(1)

random_polygon <- random_point %>%
  point_to_polygon(radius = 250, endCapStyle = "SQUARE") %>%
  sf::st_transform(sf::st_crs(s2_10))

x <- s2_20[random_polygon] %>% methods::as("Raster")
x <- x[[seq(6)]] %>% stars::st_as_stars(proxy = FALSE)

