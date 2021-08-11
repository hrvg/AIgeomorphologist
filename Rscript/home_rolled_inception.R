devtools::load_all()
library(progressr)
handlers(global = TRUE)
handlers("progress")
library(keras)

img_dir <- "/media/hguillon/datadrive/deep_learning_images_naip/" # output folder
deep_learning_images <- deep_learning_images_naip %>% na.omit()

labels <- deep_learning_images %>%
  dplyr::pull(.data$label) %>%
  gsub("CA-", "", .) %>%
  as.numeric()

class_names <- labels %>% unique() %>% sort()
class_names <- paste0("CA-", class_names)

nx <- ny <- 128
all_images <- get_deep_learning_images(deep_learning_images, nx, ny, nband = seq(4))

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
    y = labels[train_ind] - 1) # python convention
  ,
  test = list(
    x = all_images[test_ind, , , ],
    y = labels[test_ind] - 1) # python convention
  )
c(train_images, train_labels) %<-% hg_data$train
c(test_images, test_labels) %<-% hg_data$test

# create the base pre-trained model
datagen <- image_data_generator(
  horizontal_flip = TRUE
)

batch_size <- 16
datagen %>% fit_image_data_generator(train_images)
training_image_flow <- flow_images_from_data(train_images, train_labels, datagen, batch_size = batch_size)
validation_image_flow <- flow_images_from_data(test_images, test_labels, datagen, batch_size = batch_size)

inception_module <- function(input_tensor) {
  tower_1 <- input_tensor %>% 
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding='same', activation='relu') %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding='same', activation='relu')
  tower_2 <- input_tensor %>% 
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding='same', activation='relu') %>% 
    layer_conv_2d(filters = 64, kernel_size = c(5, 5), padding='same', activation='relu')
  tower_3 <- input_tensor %>% 
    layer_max_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>% 
    layer_conv_2d(filters = 64, kernel_size = c(1, 1), padding='same', activation='relu')
  layer_out <- layer_concatenate(list(tower_1, tower_2, tower_3), axis = 1)
  return(layer_out)
}

visible <- layer_input(shape = dim(train_images) %>% tail(3))
output <- visible %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu') %>%
  inception_module() %>%
  layer_max_pooling_2d() %>% 
  inception_module() %>%
  layer_global_average_pooling_2d() %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 32, activation = 'relu') %>% 
  layer_dense(units = 10, activation = 'softmax')
model <- keras_model(visible, output)

summary(model)

# create custom metric to wrap metric with parameter
metric_top_1_categorical_accuracy <-
  custom_metric("top_1_categorical_accuracy", function(y_true, y_pred) {
    metric_top_k_categorical_accuracy(y_true, y_pred, k = 1)
})

# create custom metric to wrap metric with parameter
metric_top_3_categorical_accuracy <-
  custom_metric("top_3_categorical_accuracy", function(y_true, y_pred) {
    metric_top_k_categorical_accuracy(y_true, y_pred, k = 3)
})

# create custom metric to wrap metric with parameter
metric_top_5_categorical_accuracy <-
  custom_metric("top_5_categorical_accuracy", function(y_true, y_pred) {
    metric_top_k_categorical_accuracy(y_true, y_pred, k = 5)
})

# compile the model (should be done *after* setting layers to non-trainable)
model %>% compile(
  optimizer = optimizer_nadam(), 
  loss = "sparse_categorical_crossentropy",
  metrics = c(
    "accuracy", 
    metric_top_1_categorical_accuracy,
    metric_top_3_categorical_accuracy,
    metric_top_5_categorical_accuracy
  )
)

n_epochs <- 30
history <- model %>% fit(
  training_image_flow,
  epochs = n_epochs,
  steps_per_epoch = training_image_flow$n / training_image_flow$batch_size,
  validation_data = validation_image_flow,
  validation_steps = validation_image_flow$n / validation_image_flow$batch_size
)
