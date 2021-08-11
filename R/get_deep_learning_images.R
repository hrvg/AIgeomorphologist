#' Get deep learning images
#' 
#' @param deep_learning_images a `data.frame` with column `fpath`
#' @param nx `numeric` output dimension in x
#' @param ny `numeric` output dimension in y
#' @param nband `numeric` output dimension in depth
#' @export
get_deep_learning_images <- function(deep_learning_images, nx, ny, nband = seq(3)){
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
    # img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = c(13,14,15))) %>%
    # starsExtra::rgb_to_greyscale()
    # img <- img$greyscale
    img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = nband))[[1]]
    # img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = seq(12)))[[1]]
    img[is.na(img)] <- 0
    if(all(img == 0)) return(NA)
    number_rows <- nrow(img)
    number_cols <- ncol(img)
    mid_row <- number_rows %/% 2
    mid_col <- number_cols %/% 2
    ix <- seq(mid_row - nx %/% 2, mid_row + nx %/% 2 - 1)
    iy <- seq(mid_col - ny %/% 2, mid_col + ny %/% 2 - 1)
    img <- tryCatch(img[ix, iy, ], error = function(e) e, warning = function(w) w)
    p(sprintf("i=%g", i))
    if (is(img, "warning") | is(img, "error")) {
      return(NA)
    } else {
      return(img)
    }
  }
  future::plan(future::sequential)
  return(all_images)
}
