devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")

naip_dir <- "/media/hguillon/UCD_eFlow_BigData1/hguillon/research/data/california-rivers/USGS_data/aerial-photography"
naip_fname <- "NAIP_mosaic_2016.tif"

out_dir  <- "/media/hguillon/datadrive/deep_learning_images_naip/" # output folder

# add id_tiles to labelled_points
labelled_points <- SSCT_data %>% to_spatial() %>% sf::st_as_sf()
ind <- sapply(sf::st_intersects(labelled_points, sen2r::s2_tiles()), function(z) if (length(z)==0) NA_integer_ else z[1])
id_tiles <- sen2r::s2_tiles()[ind, ] %>% dplyr::pull(tile_id)
labelled_points <- labelled_points %>% 
  dplyr::mutate(id_tile = id_tiles) %>%
  dplyr::mutate(ward = sapply(.data$ward, function(x) paste0("CA-", x))) %>%
  dplyr::rename(label = .data$ward)

# create outdir
if(!dir.exists(out_dir)) dir.create(out_dir)

  # create subdirs
for(x in unique(labelled_points$label)){
  dname <- file.path(out_dir, x)
  if(!dir.exists(dname)) dir.create(dname)
}

naip_mosaic <- stars::read_stars(file.path(naip_dir, naip_fname))

extract_images_naip <- function(naip_mosaic, labelled_points, radius) {
  current_tile <- "NAIP"
  year <- 2016

  naip_bbox <- naip_mosaic %>% sf::st_bbox() %>% sf::st_as_sfc()

  labelled_points <- labelled_points %>%
  sf::st_transform(sf::st_crs(naip_mosaic))

  labelled_polygons <- lapply(seq(nrow(labelled_points)), function(i) {
  current_point <- labelled_points[i, ]
  current_point %>%
    sf::st_buffer(radius, endCapStyle = "SQUARE") %>%
    sf::st_geometry()
  })

  all_bbox <- lapply(labelled_polygons, function(current_polygon) {
  current_polygon %>% sf::st_bbox() %>% sf::st_as_sfc()
  })

  crop_flags <- sapply(all_bbox, function(current_bbox) {
  sf::st_within(current_bbox, naip_bbox, sparse = FALSE)[, 1]
  })

  print(table(crop_flags))

  # # setting up parallelization
  # doFuture::registerDoFuture()
  # n_cores <- future::availableCores() - 2
  # if(.Platform$OS.type == "unix"){
  #   future::plan(future::multicore, workers = n_cores)
  # } else {
  #   future::plan(future::multisession, workers = n_cores)
  # }
  p <- progressr::progressor(along = seq(nrow(labelled_points)))
  res <- foreach(i = seq(nrow(labelled_points)), .combine = rbind) %do% {
    current_point <- labelled_points[i, ]
    fpath <- file.path(
      out_dir,
      current_point$label,
      paste0(
        current_point$SiteID,
        "_",
        year,
        "_",
        current_tile,".tif"
        )
      )
    if (!file.exists(fpath)) {
      current_polygon <- current_point %>%
        sf::st_buffer(radius, endCapStyle = "SQUARE") %>%
        sf::st_geometry()
      current_bbox <- current_polygon %>% sf::st_bbox() %>% sf::st_as_sfc()
      crop_flag <- sf::st_within(current_bbox, naip_bbox, sparse = FALSE)[, 1]
      if (crop_flag) {
          cropped_naip <- stars::st_as_stars(naip_mosaic[current_polygon])
          stars::write_stars(cropped_naip, fpath)
      } else {
        fpath = NA
      }
    }
    df <- data.frame(
      SiteID = current_point$SiteID, 
      label = current_point$label, 
      dop = year,
      tile_id = current_tile,
      fpath = fpath
      )
    p(sprintf("i=%g", i))
    return(df)
  }
  # future::plan(future::sequential)
  return(res)
}

deep_learning_images_naip <- extract_images_naip(naip_mosaic, labelled_points, 512)

length(list.files(out_dir, recursive = TRUE, pattern = ".tif"))

saveRDS(deep_learning_images_naip, file.path(out_dir, "deep_learning_images_naip.Rds"))
usethis::use_data(deep_learning_images_naip, overwrite = TRUE)

# diagnostic

deep_learning_images_naip$fpath %>% 
sample(1) %>%
stars::read_stars() %>% 
plot()