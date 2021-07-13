devtools::load_all()
library(foreach)
library(doFuture)
library(progressr)

# init
safe_dir <- "/media/hguillon/UCD eFlow BigData1/S2_data/SAFE/" # folder to store downloaded SAFE
out_dir  <- "/media/hguillon/datadrive/deep_learning_images/" # output folder

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

# deep_learning_images <- extract_images(
#   safe_dir,
#   c("11SKA"),
#   # id_tiles %>% unique(),
#   labelled_points,
#   .radius = 640
#   )

id_tiles <- id_tiles %>% unique()
.radius = 640

 safe_dirs <- list.dirs(safe_dir, full.names = TRUE, recursive = FALSE)

  # setting up parallelization
  doFuture::registerDoFuture()
  n_cores <- future::availableCores() - 2
  if(.Platform$OS.type == "unix"){
    future::plan(future::multicore, workers = n_cores)
  } else {
    future::plan(future::multisession, workers = n_cores)
  }

  pb_tiles <- progress::progress_bar$new(
    format = "[:bar] :current/:total tiles - :percent in :elapsed/:eta (:elapsedfull) \n",
    total = length(id_tiles),
    show_after = 0
    )
  invisible(pb_tiles$tick(0))
  res <- foreach(k = seq_along(id_tiles), .combine = rbind) %do% {
    current_tile <- id_tiles[[k]]
    current_dirs <- safe_dirs[grepl(current_tile, safe_dirs)]
    pb_dirs <- progress::progress_bar$new(
    format = "[:bar] :current/:total archives - :percent in :elapsed/:eta (:elapsedfull) \n",
    total = length(current_dirs),
    show_after = 0
    )
    invisible(pb_dirs$tick(0))
    res <- foreach(j = seq_along(current_dirs), .combine = rbind) %do% {
      safe_dir <- current_dirs[[j]]
      dop <- sen2r::safe_getMetadata(safe_dir, info = "fileinfo") %>% 
        dplyr::pull(.data$sensing_datetime) %>%
        as.Date()
      # "B4" "B3" "B2" "B8"
      .s2_10 <- stars::read_stars(
        file.path(safe_dir, "MTD_MSIL2A.xml"),
        proxy = TRUE,
        quiet = TRUE,
        sub = 1
        )
      # "B5"  "B6"  "B7"  "B8A" "B11" "B12" "AOT" "CLD" "SCL" "SNW" "WVP"
      .s2_20 <- stars::read_stars(
        file.path(safe_dir, "MTD_MSIL2A.xml"),
        proxy = TRUE,
        quiet = TRUE,
        sub = 2
        ) 
      # "B1"  "B9"  "AOT" "CLD" "SCL" "SNW" "WVP"
      .s2_60 <- stars::read_stars(
        file.path(safe_dir, "MTD_MSIL2A.xml"),
        proxy = TRUE,
        quiet = TRUE,
        sub = 3
        )
      # red, green, blue
      .s2_tci <- stars::read_stars(
        file.path(safe_dir, "MTD_MSIL2A.xml"),
        proxy = TRUE,
        quiet = TRUE,
        sub = 4
        )
      points_in_tile <- labelled_points %>% dplyr::filter(id_tile == current_tile)
      res <- foreach(i = seq(nrow(points_in_tile)), .combine = rbind) %dopar% {
        current_point <- points_in_tile[i, ]
        fpath <- file.path(
          out_dir,
          current_point$label,
          paste0(
            current_point$SiteID,
            "_",
            dop,
            "_",
            current_tile,".tif"
            )
          )
        if (!file.exists(fpath)) {
          current_polygon <- current_point %>%
          point_to_polygon(radius = .radius, endCapStyle = "SQUARE") %>%
          sf::st_transform(sf::st_crs(.s2_10))
          s2_10 <- .s2_10[current_polygon] %>% stars::st_as_stars()
          s2_20 <- .s2_20[current_polygon] %>% stars::st_as_stars() 
          s2_20 <- s2_20[,,,seq(6)] %>% stars::st_warp(s2_10)
          s2_60 <- .s2_60[current_polygon] %>% stars::st_as_stars() 
          s2_60 <- s2_60[,,,seq(2)] %>% stars::st_warp(s2_10)
          s2_tci <- .s2_tci[current_polygon] %>% stars::st_as_stars() 
          all_s2 <- c(s2_10, s2_20, s2_60, s2_tci, along = "band")
          stars::write_stars(all_s2, fpath)
        }
        df <- data.frame(
          SiteID = current_point$SiteID, 
          label = current_point$label, 
          dop = dop,
          tile_id = current_tile,
          fpath = fpath
          )
        return(df)
      }
      pb_dirs$tick()
      return(res)
    }
    pb_tiles$tick()
    return(res)
  } 
  future::plan(future::sequential)
  return(res)