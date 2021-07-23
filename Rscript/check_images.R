devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")

safe_dir <- "/media/hguillon/UCD_eFlow_BigData1/S2_data/SAFE/" # folder to store downloaded SAFE

#' A diagnostic function
#' 
#' @param deep_learning_images a `data.frame` with column `fpath`
#' @return `FALSE` if the data is missing, `TRUE` if all is well, `NA` if the data is too small
check_images <- function(deep_learning_images){
  # setting up parallelization
  doFuture::registerDoFuture()
  n_cores <- future::availableCores() - 2
  if(.Platform$OS.type == "unix"){
    future::plan(future::multicore, workers = n_cores)
  } else {
    future::plan(future::multisession, workers = n_cores)
  }
  p <- progressr::progressor(along = deep_learning_images$fpath)
  all_images <- foreach(i = seq_along(deep_learning_images$fpath), .inorder = TRUE) %dopar% {
    fp <- deep_learning_images$fpath[i]
    img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = c(13,14,15))) %>%
    starsExtra::rgb_to_greyscale()
    img <- img$greyscale
    img[is.na(img)] <- 0
    if(all(img == 0)) return(FALSE)
    img <- tryCatch(img[seq(128), seq(128)], error = function(e) e, warning = function(w) w)
    p(sprintf("i=%g", i))
    if (is(img, "warning") | is(img, "error")) {
      return(NA)
    } else {
      return(TRUE)
    }
  }
  future::plan(future::sequential)
  return(all_images)
}

all_images_check <- check_images(deep_learning_images)

# du -sh SAFE/* | sort -h >> safe_filesize.txt
# replace space with ,
# save as csv with column name: 
# fs,mission,level,sensing_date,baseline,orbit,tile,discriminator,ext

safe_filesize <- read.csv("/media/hguillon/UCD_eFlow_BigData1/S2_data/safe_filesize.csv") %>%
  dplyr::mutate(tile_id = sapply(.data$tile, function(x) substr(x, 2, 6))) %>%
  dplyr::mutate(dop = .data$sensing_date %>% lubridate::ymd_hms() %>% as.Date())

ind_untrue <- which(!(all_images_check %>% unlist()) == TRUE)


all(
  deep_learning_images$SiteID %>% unique() 
  %in% 
  deep_learning_images[-ind_untrue, ]$SiteID %>% unique()
)
deep_learning_images[-ind_untrue, ]$SiteID %>% table() %>% sort()

safe_with_issues <- dplyr::left_join(
  deep_learning_images[ind_untrue, ],
  safe_filesize,
  by = c("tile_id", "dop")) %>%
  dplyr::select(-c(.data$SiteID, .data$label, .data$fpath)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    safe_path = paste(
      .data$mission,
      .data$level,
      .data$sensing_date,
      .data$baseline,
      .data$orbit,
      .data$tile,
      .data$discriminator,
      sep = "_"
      )
    ) %>%
  dplyr::mutate(safe_path = paste0(safe_path, .data$ext)) %>%
  dplyr::mutate(safe_path = file.path(safe_dir, safe_path)) %>%
  dplyr::mutate(ctime = sapply(
    safe_path,
    function(fp) file.info(fp)$ctime %>% as.Date() %>% as.character() %>% as.factor())
  ) %>%
  dplyr::mutate(download_m = lubridate::ymd(as.character(ctime)) %>% lubridate::month())

usethis::use_data(ind_untrue)