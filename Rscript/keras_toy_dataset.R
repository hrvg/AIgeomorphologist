devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")

#' Get deep learning images
#' 
#' @param deep_learning_images a `data.frame` with column `fpath`
get_deep_learning_images <- function(deep_learning_images){
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
    img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = c(13,14,15)))[[1]]
    # img <- stars::read_stars(fp, proxy = FALSE, RasterIO = list(bands = seq(12)))[[1]]
    img[is.na(img)] <- 0
    if(all(img == 0)) return(NA)
    img <- tryCatch(img[seq(128), seq(128), ], error = function(e) e, warning = function(w) w)
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

N_SAMPLE <- 1
toy_deep_learning_images <- deep_learning_images[-ind_untrue, ] %>%
  dplyr::group_by(.data$SiteID) %>%
  dplyr::group_modify(~ {dplyr::sample_n(.x, N_SAMPLE)})

labels <- toy_deep_learning_images %>%
  dplyr::pull(.data$label) %>%
  gsub("CA-", "", .) %>%
  as.numeric()


class_names <- labels %>% unique() %>% sort()
class_names <- paste0("CA-", class_names)

all_images <- get_deep_learning_images(toy_deep_learning_images)


ind_na <- which(all_images %>% is.na)
labels <- labels[-ind_na]
if (length(ind_na) > 1) {
  all_images <- rlist::list.remove(all_images, ind_na)
}
input_dim <- dim(all_images[[1]])
all_images <- abind::abind(all_images, along = 0)
dim(all_images)

# trad way
split_ratio <- 0.8
ind <- seq_along(labels)
train_ind <- sample(ind, split_ratio * length(labels))
test_ind <- ind[-train_ind]
table(labels[train_ind])

# stratified holdout
split_ratio <- 0.8
block_cv <- toy_deep_learning_images[-ind_na, ] %>%
  tibble::rownames_to_column() %>%
  dplyr::group_by(.data$label) %>%
  dplyr::sample_frac(split_ratio)
train_ind <- block_cv$rowname %>% as.numeric()
test_ind <- seq_along(labels)[-train_ind]
table(labels[train_ind])

# stratified block holdout
split_ratio <- 0.8
block_cv <- toy_deep_learning_images[-ind_na, ] %>%
  tibble::rownames_to_column() %>%
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

library(keras)

c(train_images, train_labels) %<-% hg_data$train
c(test_images, test_labels) %<-% hg_data$test
# train_images <- train_images / 255
# test_images <- test_images / 255

# dir.create("tmp")
# train_augmented <- flow_images_from_data(
#   train_images, 
#   y = train_labels,
#   generator = image_data_generator(
#       horizontal_flip = TRUE,
#       vertical_flip = TRUE,
#       rotation_range = 180,
#       fill_mode = "constant",
#       cval = 0
#     ),
#   save_to_dir = NULL
#   )

# test_augmented <- flow_images_from_data(
#   test_images, 
#   y = test_labels,
#   generator = image_data_generator(
#       horizontal_flip = TRUE,
#       vertical_flip = TRUE,
#       rotation_range = 180,
#       fill_mode = "constant",
#       cval = 0
#     ),
#   save_to_dir = NULL
#   )

# par(mfcol=c(5,5))
# par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
# for (i in 1:25) { 
#   img <- train_images[i, , , 1]
#   img <- t(apply(img, 2, rev)) 
#   image(1:128, 1:128, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
#         main = paste(class_names[train_labels[i] + 1]))
# }

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', 
                input_shape = input_dim) %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_dropout(rate = 0.25)

model %>%
  layer_flatten() %>% 
  layer_dense(units = 256, activation = 'relu') %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% 
  compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

history <- model %>% 
  fit(
    # train_augmented,
    x = hg_data$train$x, 
    y = hg_data$train$y,
    epochs = 1000,
    # validation_data = test_augmented,
    validation_data = hg_data$test,
    verbose = 1
  )

plot(history)
evaluate(model, hg_data$test$x, hg_data$test$y, verbose = 0)

model %>% fit(train_images, train_labels, epochs = 100, verbose = 2)
score <- model %>% evaluate(test_images, test_labels, verbose = 0)
score

predictions <- model %>% predict(test_images)
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) { 
  img <- test_images[i, , ]
  img <- t(apply(img, 2, rev)) 
  # subtract 1 as labels go from 0 to 9
  predicted_label <- which.max(predictions[i, ]) - 1
  true_label <- test_labels[i]
  if (predicted_label == true_label) {
    color <- '#008800' 
  } else {
    color <- '#bb0000'
  }
  image(1:128, 1:128, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1], " (",
                      class_names[true_label + 1], ")"),
        col.main = color)
}