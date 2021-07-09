#' Extract images from Sentinel-2 archice
#' @param safe_dir `character`, path to the SAFE archives
#' @param id_tiles `character`, list of S2 tiles
#' @param labelled_points `sf`, points with a `label`
#' @param .radius `numeric`, half-dimension of the cropped image in map units
#' @return a `data.frame` with information about the SiteID and file paths
#' @export
#' @import foreach
#' @import doFuture
extract_images <- function(safe_dir, id_tiles, labelled_points, .radius = 640){
  # parse dir
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
    format = "[:bar] :current/:total - :percent in :elapsed/:eta \n",
    total = length(id_tiles),
    show_after = 0
    )
  invisible(pb_tiles$tick(0))
  res <- foreach(k = seq_along(id_tiles), .combine = rbind) %do% {
    current_tile <- id_tiles[[k]]
    current_dirs <- safe_dirs[grepl(current_tile, safe_dirs)]
    pgr_dirs <- progressr::progressor(along = current_dirs)
    res <- foreach(j = seq_along(current_dirs), .inorder = FALSE, .combine = rbind) %do% {
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
        stars::write_stars(all_s2, fpath)
        df <- data.frame(
          SiteID = current_point$SiteID, 
          label = current_point$label, 
          dop = dop,
          tile_id = current_tile,
          fpath = fpath
          )
        return(df)
      }
      pgr_dirs(sprintf("j=%g", j))
      return(res)
    }
    pb_tiles$tick()
    return(res)
  } 
  future::plan(future::sequential)
  return(res)
}