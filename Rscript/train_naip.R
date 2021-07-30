devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")
library(keras)

img_dir <- "/media/hguillon/datadrive/deep_learning_images_naip/" # output folder

deep_learning_images <- deep_learning_images_naip %>% na.omit()

#' Get deep learning images
#' 
#' @param deep_learning_images a `data.frame` with column `fpath`
get_deep_learning_images <- function(deep_learning_images, nx, ny){
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
    img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = seq(3)))[[1]]
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


labels <- deep_learning_images %>%
  dplyr::pull(.data$label) %>%
  gsub("CA-", "", .) %>%
  as.numeric()

class_names <- labels %>% unique() %>% sort()
class_names <- paste0("CA-", class_names)

nx <- ny <- 384
all_images <- get_deep_learning_images(deep_learning_images, nx, ny)

ind_na <- which(all_images %>% is.na)
labels <- labels[-ind_na]
if (length(ind_na) >= 1) {
  all_images <- rlist::list.remove(all_images, ind_na)
}
input_dim <- dim(all_images[[1]])
all_images <- abind::abind(all_images, along = 0)
dim(all_images)

# stratified block holdout
split_ratio <- 0.8
block_cv <- deep_learning_images[-ind_na, ] %>%
  # tibble::rownames_to_column() %>%
  dplyr::mutate(rowname = seq(nrow(.))) %>%
  dplyr::group_by(.data$label) %>%
  dplyr::group_modify(~ {
    to_select <- .x$SiteID %>% unique()
    to_select <- to_select %>% sample(split_ratio * length(to_select))
    .x %>% dplyr::filter(.data$SiteID %in% to_select)
  })
train_ind <- block_cv$rowname %>% as.numeric()
test_ind <- seq_along(labels)[-train_ind]
table(labels[train_ind])


hg_data <- list(
  train = list(
    x = all_images[train_ind, , , ],
    y = labels[train_ind] - 1)
  ,
  test = list(
    x = all_images[test_ind, , , ],
    y = labels[test_ind] - 1)
  )
c(train_images, train_labels) %<-% hg_data$train
c(test_images, test_labels) %<-% hg_data$test

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', 
                input_shape = input_dim) %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.25)

model %>%
  layer_flatten() %>% 
  layer_dense(units = 512, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% 
  compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

datagen <- image_data_generator(
  horizontal_flip = TRUE,
  vertical_flip = TRUE
  )

datagen %>% fit_image_data_generator(train_images)

batch_size <- 8
epochs <- 250
n <- dim(train_images)[1]


train_data <- flow_images_from_data(train_images, train_labels, datagen, batch_size = batch_size)

unlink("tmp")
dir.create("tmp")
test_data <- flow_images_from_data(test_images, test_labels, datagen, batch_size = batch_size, 
  save_to_dir = NULL)

history <- model %>% fit(
  train_data,
  steps_per_epoch = as.integer(n / batch_size), 
  epochs = epochs, 
  validation_data = test_data
)
