devtools::load_all()
library(foreach)
library(doFuture)
library(progressr)

if (as.numeric(R.version$major) >= 4) {
  progressr::handlers(global = TRUE)
  progressr::handlers(list(
    progressr::handler_progress(
      format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
      width    = 60,
      complete = "+"
      )
    )
  )
}

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

deep_learning_images <- extract_images(
  safe_dir,
  id_tiles %>% unique(),
  labelled_points,
  .radius = 640
  )